package unused2

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"os"
	"reflect"
	"strings"

	"golang.org/x/exp/typeparams"
	"golang.org/x/tools/go/analysis"
	"honnef.co/go/tools/analysis/facts/directives"
	"honnef.co/go/tools/analysis/facts/generated"
	"honnef.co/go/tools/analysis/lint"
	"honnef.co/go/tools/go/ast/astutil"
	"honnef.co/go/tools/go/types/typeutil"
	"honnef.co/go/tools/unused"
)

// TODO(dh): currently, types use methods that implement interfaces. However, this makes a method used even if the
// relevant interface is never used. What if instead interfaces used those methods? Right now we cannot do that, because
// methods use their receivers, so using a method uses the type. But do we need that edge? Is there a way to refer to a
// method without explicitly mentioning the type somewhere? If not, the edge from method to receiver is superfluous.

// XXX embedded fields that contribute exported methods or fields should be considered used (6.4 and 6.5)
// XXX embedded exported types should be used, their name is exported

// XXX improve tests to not only check for used/unused, but also for quiet

// XXX vet all code for proper use of core types

// XXX handle implementing interfaces

// XXX aliases/function vars are temporary and will vanish once we replace unused with unused2

type Result = unused.Result
type SerializedResult = unused.SerializedResult
type SerializedObject = unused.SerializedObject

var Serialize = unused.Serialize
var Implements = unused.Implements

func debugf(f string, v ...interface{}) {
	// XXX respect -debug.unused-graph flag for destination
	fmt.Fprintf(os.Stdout, f, v...)
}

// XXX all of these are only exported so we can refer to them in unused2, unexport them when we replace unused with
// unused2

var typString = unused.TypString
var isIrrelevant = unused.IsIrrelevant
var isNoCopyType = unused.IsNoCopyType
var runtimeFuncs = unused.RuntimeFuncs

var Analyzer = &lint.Analyzer{
	Doc: &lint.Documentation{
		Title: "Unused code",
	},
	Analyzer: &analysis.Analyzer{
		Name:       "U1000",
		Doc:        "Unused code",
		Run:        run,
		Requires:   []*analysis.Analyzer{generated.Analyzer, directives.Analyzer},
		ResultType: reflect.TypeOf(Result{}),
	},
}

type graph struct {
	Root        *node
	Nodes       map[types.Object]*node
	pass        *analysis.Pass
	nodeCounter uint64

	// package-level named types
	namedTypes     []*types.TypeName
	interfaceTypes []*types.Interface
}

func (g *graph) newNode(obj types.Object) *node {
	g.nodeCounter++
	return &node{
		obj: obj,
		id:  g.nodeCounter,
	}
}

func (g *graph) node(obj types.Object) *node {
	if obj == nil {
		return g.Root
	}
	obj = origin(obj)
	if n, ok := g.Nodes[obj]; ok {
		return n
	}
	n := g.newNode(obj)
	g.Nodes[obj] = n
	return n
}

type node struct {
	// OPT(dh): we could trivially turn this from AoS into SoA. Benchmark if that has any benefits.
	// OPT(dh): we can put the id, seen, and quiet into a single 64 bit variable, with 62 bits for the ID
	// OPT(dh): ^ a 32 bit variable would probably suffice. 30 bits would allow for a billion different objects, which would, at a minimum, require a 3 GB large source file.
	// OPT(dh): ^ benchmark if these changes have any measurable effect on CPU or memory usage

	obj types.Object
	id  uint64

	// OPT(dh): evaluate using a map instead of a slice to avoid
	// duplicate edges.
	uses []*node
	owns []*node

	// set during final graph walk if node is reachable
	seen  bool
	quiet bool
}

func origin(obj types.Object) types.Object {
	// XXX this depends on Go 1.19. How can we achieve the same in 1.18?
	switch obj := obj.(type) {
	case *types.Var:
		return obj.Origin()
	case *types.Func:
		return obj.Origin()
	default:
		return obj
	}
}

func (g *graph) see(obj, owner types.Object) {
	if obj == nil {
		panic("saw nil object")
	}

	// XXX don't track objects in other packages
	// XXX use isIrrelevant

	nObj := g.node(obj)
	if owner != nil {
		nOwner := g.node(owner)
		nOwner.owns = append(nOwner.owns, nObj)
	}
}

func ourIsIrrelevant(obj types.Object) bool {
	// XXX rename this function
	switch obj.(type) {
	case *types.PkgName:
		return true
	default:
		return false
	}
}

func (g *graph) use(used, by types.Object) {
	if used.Pkg() != g.pass.Pkg {
		return
	}

	if ourIsIrrelevant(used) {
		return
	}

	nUsed := g.node(used)
	nBy := g.node(by)
	nBy.uses = append(nBy.uses, nUsed)
}

func (g *graph) color(root *node) {
	if root.seen {
		return
	}
	root.seen = true
	for _, n := range root.uses {
		g.color(n)
	}
}

func (g *graph) entry(pass *analysis.Pass) {
	for _, f := range pass.Files {
		for _, cg := range f.Comments {
			for _, c := range cg.List {
				if strings.HasPrefix(c.Text, "//go:linkname ") {
					// FIXME(dh): we're looking at all comments. The
					// compiler only looks at comments in the
					// left-most column. The intention probably is to
					// only look at top-level comments.

					// (1.8) packages use symbols linked via go:linkname
					fields := strings.Fields(c.Text)
					if len(fields) == 3 {
						obj := pass.Pkg.Scope().Lookup(fields[1])
						if obj == nil {
							continue
						}
						g.use(obj, nil)
					}
				}
			}
		}
	}

	for _, f := range pass.Files {
		for _, decl := range f.Decls {
			g.decl(decl, nil)
		}
	}

	processMethodSet := func(named *types.TypeName, ms *types.MethodSet) {
		for i := 0; i < ms.Len(); i++ {
			// (2.1) named types use exported methods
			// (6.4) structs use embedded fields that have exported methods
			//
			// By reading the selection, we read all embedded fields that are part of the path
			m := ms.At(i)
			if token.IsExported(m.Obj().Name()) {
				g.readSelection(m, named)
			}
		}

		if _, ok := named.Type().Underlying().(*types.Interface); !ok {
			// (8.0) handle interfaces
			//
			// We don't care about interfaces implementing interfaces; all their methods are already used, anyway
			for _, iface := range g.interfaceTypes {
				if sels, ok := Implements(named.Type(), iface, ms); ok {
					for _, sel := range sels {
						// (8.2) any concrete type implements all known interfaces
						// (6.3) structs use embedded fields that help implement interfaces
						g.readSelection(sel, named)
					}
				}
			}
		}
	}

	for _, named := range g.namedTypes {
		// OPT(dh): do we already have the method set available?
		processMethodSet(named, types.NewMethodSet(named.Type()))
		processMethodSet(named, types.NewMethodSet(types.NewPointer(named.Type())))

	}

	// XXX handle ignore directives
	type ignoredKey struct {
		file string
		line int
	}
	ignores := map[ignoredKey]struct{}{}
	directives := pass.ResultOf[directives.Analyzer].([]lint.Directive)
	for _, dir := range directives {
		if dir.Command != "ignore" && dir.Command != "file-ignore" {
			continue
		}
		if len(dir.Arguments) == 0 {
			continue
		}
		for _, check := range strings.Split(dir.Arguments[0], ",") {
			if check == "U1000" {
				pos := pass.Fset.PositionFor(dir.Node.Pos(), false)
				var key ignoredKey
				switch dir.Command {
				case "ignore":
					key = ignoredKey{
						pos.Filename,
						pos.Line,
					}
				case "file-ignore":
					key = ignoredKey{
						pos.Filename,
						-1,
					}
				}

				ignores[key] = struct{}{}
				break
			}
		}
	}

	if len(ignores) > 0 {
		// all objects annotated with a //lint:ignore U1000 are considered used
		for obj := range g.Nodes {
			pos := pass.Fset.PositionFor(obj.Pos(), false)
			key1 := ignoredKey{
				pos.Filename,
				pos.Line,
			}
			key2 := ignoredKey{
				pos.Filename,
				-1,
			}
			_, ok := ignores[key1]
			if !ok {
				_, ok = ignores[key2]
			}
			if ok {
				g.use(obj, nil)

				// use methods and fields of ignored types
				if obj, ok := obj.(*types.TypeName); ok {
					if obj.IsAlias() {
						if typ, ok := obj.Type().(*types.Named); ok && typ.Obj().Pkg() != obj.Pkg() {
							// This is an alias of a named type in another package.
							// Don't walk its fields or methods; we don't have to.
							//
							// For aliases to types in the same package, we do want to ignore the fields and methods,
							// because ignoring the alias should ignore the aliased type.
							continue
						}
					}
					if typ, ok := obj.Type().(*types.Named); ok {
						for i := 0; i < typ.NumMethods(); i++ {
							g.use(typ.Method(i), nil)
						}
					}
					if typ, ok := obj.Type().Underlying().(*types.Struct); ok {
						for i := 0; i < typ.NumFields(); i++ {
							g.use(typ.Field(i), nil)
						}
					}
				}
			}
		}
	}
}

func isOfType[T any](x any) bool {
	_, ok := x.(T)
	return ok
}

func (g *graph) read(node ast.Node, by types.Object) {
	if node == nil {
		return
	}

	switch node := node.(type) {
	case *ast.Ident:
		// XXX this branch, in the end, handles all uses of objects
		// (7.2) field accesses use fields

		obj := g.pass.TypesInfo.ObjectOf(node)
		g.use(obj, by)

	case *ast.BasicLit:
		// Nothing to do

	case *ast.SliceExpr:
		g.read(node.X, by)
		g.read(node.Low, by)
		g.read(node.High, by)
		g.read(node.Max, by)

	case *ast.UnaryExpr:
		g.read(node.X, by)

	case *ast.ParenExpr:
		g.read(node.X, by)

	case *ast.ArrayType:
		g.read(node.Len, by)
		g.read(node.Elt, by)

	case *ast.SelectorExpr:
		g.readSelectorExpr(node, by)

	case *ast.IndexExpr:
		g.read(node.X, by)
		g.read(node.Index, by)

	case *ast.BinaryExpr:
		g.read(node.X, by)
		g.read(node.Y, by)

	case *ast.CompositeLit:
		g.read(node.Type, by)
		// We get the type of the node itself, not of node.Type, to handle nested composite literals of the kind
		// T{{...}}
		typ, isStruct := typeutil.CoreType(g.pass.TypesInfo.TypeOf(node)).(*types.Struct)

		if isStruct && len(node.Elts) != 0 && !isOfType[*ast.KeyValueExpr](node.Elts[0]) {
			// Untagged struct literal that specifies all fields
			for i := 0; i < typ.NumFields(); i++ {
				g.use(typ.Field(i), by)
			}
		}
		for _, elt := range node.Elts {
			g.read(elt, by)
		}

	case *ast.KeyValueExpr:
		g.read(node.Key, by)
		g.read(node.Value, by)

	case *ast.StarExpr:
		g.read(node.X, by)

	case *ast.MapType:
		g.read(node.Key, by)
		g.read(node.Value, by)

	case *ast.FuncLit:
		g.read(node.Type, by)
		g.block(node.Body, by)

	case *ast.FuncType:
		g.read(node.TypeParams, by)
		g.read(node.Params, by)
		g.read(node.Results, by)

	case *ast.FieldList:
		if node == nil {
			return
		}

		// This branch is only hit for field lists enclosed by parentheses or square brackets, i.e. parameters. Fields
		// (for structs) and method lists (for interfaces) are handled elsewhere.

		for _, field := range node.List {
			if len(field.Names) == 0 {
				g.read(field.Type, by)
			} else {
				for _, name := range field.Names {
					// OPT(dh): instead of by -> name -> type, we could just emit by -> type. We don't care about the
					// (un)usedness of parameters of any kind.
					obj := g.pass.TypesInfo.ObjectOf(name)
					g.see(obj, by)
					g.use(obj, by)
					g.read(field.Type, obj)
				}
			}
		}

	case *ast.ChanType:
		g.read(node.Value, by)

	case *ast.StructType:
		// This is only used for anonymous struct types, not named ones.

		for _, field := range node.Fields.List {
			if len(field.Names) == 0 {
				// embedded field

				f := g.embeddedField(field.Type)
				g.use(f, by)
			} else {
				for _, name := range field.Names {
					// (11.1) anonymous struct types use all their fields
					// OPT(dh): instead of by -> name -> type, we could just emit by -> type. If the type is used, then the fields are used.
					obj := g.pass.TypesInfo.ObjectOf(name)
					g.see(obj, by)
					g.use(obj, by)
					g.read(field.Type, g.pass.TypesInfo.ObjectOf(name))
				}
			}
		}

	case *ast.TypeAssertExpr:
		g.read(node.X, by)
		g.read(node.Type, by)

	case *ast.InterfaceType:
		if len(node.Methods.List) != 0 {
			g.interfaceTypes = append(g.interfaceTypes, g.pass.TypesInfo.TypeOf(node).(*types.Interface))
		}
		for _, meth := range node.Methods.List {
			switch len(meth.Names) {
			case 0:
				// Embedded type or type union
				// (8.4) all embedded interfaces are marked as used
				// (this also covers type sets)

				g.read(meth.Type, by)
			case 1:
				// Method
				// (8.3) all interface methods are marked as used
				obj := g.pass.TypesInfo.ObjectOf(meth.Names[0])
				g.see(obj, by)
				g.use(obj, by)
				g.read(meth.Type, obj)
			default:
				panic(fmt.Sprintf("unexpected number of names: %d", len(meth.Names)))
			}
		}

	case *ast.Ellipsis:
		g.read(node.Elt, by)

	case *ast.CallExpr:
		g.read(node.Fun, by)
		for _, arg := range node.Args {
			g.read(arg, by)
		}

		// Handle conversiosn
		conv := node
		if len(conv.Args) != 1 || conv.Ellipsis.IsValid() {
			return
		}

		dst := g.pass.TypesInfo.TypeOf(conv.Fun)
		src := g.pass.TypesInfo.TypeOf(conv.Args[0])

		// XXX use DereferenceR instead
		// XXX guard against infinite recursion in DereferenceR
		tSrc := typeutil.CoreType(typeutil.Dereference(src))
		tDst := typeutil.CoreType(typeutil.Dereference(dst))
		stSrc, okSrc := tSrc.(*types.Struct)
		stDst, okDst := tDst.(*types.Struct)
		if okDst && okSrc {
			// Converting between two structs. The fields are
			// relevant for the conversion, but only if the
			// fields are also used outside of the conversion.
			// Mark fields as used by each other.

			assert(stDst.NumFields() == stSrc.NumFields())
			for i := 0; i < stDst.NumFields(); i++ {
				// (5.1) when converting between two equivalent structs, the fields in
				// either struct use each other. the fields are relevant for the
				// conversion, but only if the fields are also accessed outside the
				// conversion.
				g.use(stDst.Field(i), stSrc.Field(i))
				g.use(stSrc.Field(i), stDst.Field(i))
			}
		} else if okSrc && tDst == types.Typ[types.UnsafePointer] {
		} else if okDst && tSrc == types.Typ[types.UnsafePointer] {
		}

		// XXX check if dst or src are unsafe.Pointer and mark all fields as used

	default:
		lint.ExhaustiveTypeSwitch(node)
	}
}

func (g *graph) write(node ast.Node, by types.Object) {
	if node == nil {
		return
	}

	switch node := node.(type) {
	case *ast.Ident:
		obj := g.pass.TypesInfo.ObjectOf(node)
		if obj == nil {
			// This can happen for `switch x := v.(type)`, where that x doesn't have an object
			return
		}
		// g.see(obj)

		// (4.9) functions use package-level variables they assign to iff in tests (sinks for benchmarks)
		// (9.7) variable _reads_ use variables, writes do not, except in tests
		path := g.pass.Fset.File(obj.Pos()).Name()
		if strings.HasSuffix(path, "_test.go") {
			if isGlobal(obj) {
				g.use(obj, by)
			}
		}

	case *ast.IndexExpr:
		g.read(node.X, by)

	case *ast.SelectorExpr:
		g.readSelectorExpr(node, by)

	case *ast.StarExpr:
		g.read(node.X, by)

	default:
		lint.ExhaustiveTypeSwitch(node)
	}
}

// readSelectorExpr reads all elements of a selector expression, including implicit fields.
func (g *graph) readSelectorExpr(sel *ast.SelectorExpr, by types.Object) {
	// cover AST-based accesses
	g.read(sel.X, by)

	tsel, ok := g.pass.TypesInfo.Selections[sel]
	if !ok {
		return
	}
	g.readSelection(tsel, by)
}

func (g *graph) readSelection(sel *types.Selection, by types.Object) {
	indices := sel.Index()
	base := sel.Recv()
	for _, idx := range indices[:len(indices)-1] {
		// XXX do we need core types here?
		field := typeutil.Dereference(base.Underlying()).Underlying().(*types.Struct).Field(idx)
		g.use(field, by)
		base = field.Type()
	}

	g.use(sel.Obj(), by)
}

func (g *graph) block(block *ast.BlockStmt, by types.Object) {
	if block == nil {
		return
	}

	for _, stmt := range block.List {
		g.stmt(stmt, by)
	}
}

func isGlobal(obj types.Object) bool {
	return obj.Parent() == obj.Pkg().Scope()
}

func (g *graph) decl(decl ast.Decl, by types.Object) {
	switch decl := decl.(type) {
	case *ast.GenDecl:
		switch decl.Tok {
		case token.IMPORT:
			// Nothing to do

		case token.CONST:
			for _, spec := range decl.Specs {
				vspec := spec.(*ast.ValueSpec)
				assert(len(vspec.Values) == 0 || len(vspec.Values) == len(vspec.Names))
				for i, name := range vspec.Names {
					obj := g.pass.TypesInfo.ObjectOf(name)
					g.see(obj, by)
					g.read(vspec.Type, obj)

					if len(vspec.Values) != 0 {
						g.read(vspec.Values[i], obj)
					}

					if name.Name == "_" {
						g.use(obj, by)
					} else if token.IsExported(name.Name) && isGlobal(obj) {
						g.use(obj, nil)
					}
				}
			}

			groups := astutil.GroupSpecs(g.pass.Fset, decl.Specs)
			for _, group := range groups {
				// (10.1) if one constant out of a block of constants is used, mark all of them used
				//
				// We encode this as a ring. If we have a constant group 'const ( a; b; c )', then we'll produce the
				// following graph: a -> b -> c -> a.

				var first, prev, last types.Object
				for _, spec := range group {
					for _, name := range spec.(*ast.ValueSpec).Names {
						if name.Name == "_" {
							// Having a blank constant in a group doesn't mark the whole group as used
							continue
						}

						obj := g.pass.TypesInfo.ObjectOf(name)
						if first == nil {
							first = obj
						} else {
							g.use(obj, prev)
						}
						prev = obj
						last = obj
					}
				}
				if first != nil && first != last {
					g.use(first, last)
				}
			}

		case token.TYPE:
			for _, spec := range decl.Specs {
				tspec := spec.(*ast.TypeSpec)
				obj := g.pass.TypesInfo.ObjectOf(tspec.Name).(*types.TypeName)
				g.see(obj, by)
				if !tspec.Assign.IsValid() {
					g.namedTypes = append(g.namedTypes, obj)
				}
				if token.IsExported(tspec.Name.Name) && isGlobal(obj) {
					// (1.1) packages use exported named types
					g.use(g.pass.TypesInfo.ObjectOf(tspec.Name), nil)
				}

				// (2.5) named types use all their type parameters
				g.read(tspec.TypeParams, obj)

				g.namedType(obj, tspec.Type)

				if tspec.Name.Name == "_" {
					g.use(obj, by)
				}
			}

		case token.VAR:
			// We cannot rely on types.Initializer for package-level variables because
			// - initializers are only tracked for variables that are actually initialized
			// - we want to see the AST of the type, if specified, not just the rhs

			for _, spec := range decl.Specs {
				vspec := spec.(*ast.ValueSpec)
				for i, name := range vspec.Names {
					obj := g.pass.TypesInfo.ObjectOf(name)
					g.see(obj, by)
					// variables and constants use their types
					g.read(vspec.Type, obj)

					if len(vspec.Names) == len(vspec.Values) {
						// One value per variable
						g.read(vspec.Values[i], obj)
					} else if len(vspec.Values) != 0 {
						// Multiple variables initialized with a single rhs
						// assert(len(vspec.Values) == 1)
						if len(vspec.Values) != 1 {
							panic(g.pass.Fset.PositionFor(vspec.Pos(), false))
						}
						g.read(vspec.Values[0], obj)
					}

					if token.IsExported(name.Name) && isGlobal(obj) {
						// (1.3) packages use exported variables
						g.use(obj, nil)
					}

					if name.Name == "_" {
						g.use(obj, by)
					}
				}
			}

		default:
			panic(fmt.Sprintf("unexpected token %s", decl.Tok))
		}

	case *ast.FuncDecl:
		// XXX calling OriginMethod is unnecessary if we use types.Func.Origin
		obj := typeparams.OriginMethod(g.pass.TypesInfo.ObjectOf(decl.Name).(*types.Func))
		g.see(obj, by)

		if token.IsExported(decl.Name.Name) {
			if decl.Recv == nil {
				// (1.2) packages use exported functions
				g.use(obj, nil)
			}
		} else if decl.Name.Name == "init" {
			// (1.5) packages use init functions
			g.use(obj, nil)
		} else if decl.Name.Name == "main" && g.pass.Pkg.Name() == "main" {
			// (1.7) packages use the main function iff in the main package
			g.use(obj, nil)
		}

		// (4.1) functions use all their arguments, return parameters and receivers
		// (4.10) all their type parameters
		// XXX we have to g.see the receiver
		g.read(decl.Recv, obj)
		g.read(decl.Type, obj)
		g.block(decl.Body, obj)

		if decl.Name.Name == "_" {
			g.use(obj, nil)
		}

		if decl.Doc != nil {
			for _, cmt := range decl.Doc.List {
				if strings.HasPrefix(cmt.Text, "//go:cgo_export_") {
					// (1.6) packages use functions exported to cgo
					g.use(obj, nil)
				}
			}
		}

	default:
		// We do not cover BadDecl, but we shouldn't ever see one of those
		lint.ExhaustiveTypeSwitch(decl)
	}
}

func (g *graph) stmt(stmt ast.Stmt, by types.Object) {
	if stmt == nil {
		return
	}

	for {
		// We don't care about labels, so unwrap LabeledStmts. Note that a label can itself be labeled.
		if labeled, ok := stmt.(*ast.LabeledStmt); ok {
			stmt = labeled.Stmt
		} else {
			break
		}
	}

	switch stmt := stmt.(type) {
	case *ast.AssignStmt:
		if stmt.Tok == token.DEFINE {
			for _, lhs := range stmt.Lhs {
				obj := g.pass.TypesInfo.ObjectOf(lhs.(*ast.Ident))
				// obj can be nil for `switch x := v.(type)`, where that x doesn't have an object
				if obj != nil {
					defer func() {
						if err := recover(); err != nil {
							panic(g.pass.Fset.PositionFor(stmt.Pos(), false))
						}
					}()
					g.see(obj, by)
				}
			}
		}
		for _, lhs := range stmt.Lhs {
			g.write(lhs, by)
		}
		for _, rhs := range stmt.Rhs {
			// Note: it would be more accurate to have the rhs used by the lhs, but it ultimately doesn't matter,
			// because local variables are always end up used, anyway.
			g.read(rhs, by)
		}

	case *ast.BlockStmt:
		g.block(stmt, by)

	case *ast.BranchStmt:
		// Nothing to do

	case *ast.DeclStmt:
		g.decl(stmt.Decl, by)

	case *ast.DeferStmt:
		g.read(stmt.Call, by)

	case *ast.ExprStmt:
		g.read(stmt.X, by)

	case *ast.ForStmt:
		g.stmt(stmt.Init, by)
		g.read(stmt.Cond, by)
		g.stmt(stmt.Post, by)
		g.block(stmt.Body, by)

	case *ast.GoStmt:
		g.read(stmt.Call, by)

	case *ast.IfStmt:
		g.stmt(stmt.Init, by)
		g.read(stmt.Cond, by)
		g.block(stmt.Body, by)
		g.stmt(stmt.Else, by)

	case *ast.IncDecStmt:
		// Increment/decrement have no effect on the usedness of an object.

	case *ast.RangeStmt:
		g.write(stmt.Key, by)
		g.write(stmt.Value, by)
		g.read(stmt.X, by)
		g.block(stmt.Body, by)

	case *ast.ReturnStmt:
		for _, ret := range stmt.Results {
			g.read(ret, by)
		}

	case *ast.SelectStmt:
		for _, clause_ := range stmt.Body.List {
			clause := clause_.(*ast.CommClause)
			switch comm := clause.Comm.(type) {
			case *ast.SendStmt:
				g.read(comm.Chan, by)
				g.read(comm.Value, by)
			case *ast.ExprStmt:
				g.read(comm.X.(*ast.UnaryExpr).X, by)
			case *ast.AssignStmt:
				for _, lhs := range comm.Lhs {
					g.write(lhs, by)
				}
				for _, rhs := range comm.Rhs {
					g.read(rhs, by)
				}
			case nil:
			default:
				lint.ExhaustiveTypeSwitch(comm)
			}
			for _, body := range clause.Body {
				g.stmt(body, by)
			}
		}

	case *ast.SendStmt:
		g.read(stmt.Chan, by)
		g.read(stmt.Value, by)

	case *ast.SwitchStmt:
		g.stmt(stmt.Init, by)
		g.read(stmt.Tag, by)
		for _, clause_ := range stmt.Body.List {
			clause := clause_.(*ast.CaseClause)
			for _, expr := range clause.List {
				g.read(expr, by)
			}
			for _, body := range clause.Body {
				g.stmt(body, by)
			}
		}

	case *ast.TypeSwitchStmt:
		g.stmt(stmt.Init, by)
		g.stmt(stmt.Assign, by)
		for _, clause_ := range stmt.Body.List {
			clause := clause_.(*ast.CaseClause)
			for _, expr := range clause.List {
				g.read(expr, by)
			}
			for _, body := range clause.Body {
				g.stmt(body, by)
			}
		}

	case *ast.EmptyStmt:
		// Nothing to do

	default:
		lint.ExhaustiveTypeSwitch(stmt)
	}
}

// embeddedField sees the field declared by the embedded field node, and marks the type as used by the field.
//
// Embedded fields are special in two ways: they don't have names, so we don't have immediate access to an ast.Ident to
// resolve to the field's types.Var, and we cannot use g.read on the type because eventually we do get to an ast.Ident,
// and ObjectOf resolves embedded fields to the field they declare, not the type. That's why we have code specially for
// handling embedded fields.
func (g *graph) embeddedField(node ast.Node) *types.Var {
	// We need to traverse the tree to find the ast.Ident, but all the nodes we traverse should be used by the object we
	// get once we resolve the ident. Collect the nodes and process them once we've found the ident.
	nodes := make([]ast.Node, 0, 4)
	for {
		switch node_ := node.(type) {
		case *ast.Ident:
			obj := g.pass.TypesInfo.ObjectOf(node_).(*types.Var)
			for _, n := range nodes {
				g.read(n, obj)
			}
			switch typ := typeutil.Dereference(g.pass.TypesInfo.TypeOf(node_)).(type) {
			case *types.Named:
				g.use(typ.Obj(), obj)
			case *types.Basic:
				// Nothing to do
			default:
				lint.ExhaustiveTypeSwitch(typ)
			}
			return obj
		case *ast.StarExpr:
			node = node_.X
		case *ast.SelectorExpr:
			node = node_.Sel
			nodes = append(nodes, node_.X)
		case *ast.IndexExpr:
			node = node_.X
			nodes = append(nodes, node_.Index)
		case *ast.IndexListExpr:
			node = node_.X
		default:
			lint.ExhaustiveTypeSwitch(node_)
		}
	}
}

func (g *graph) namedType(typ *types.TypeName, spec ast.Expr) {
	// (2.2) named types use the type they're based on

	if st, ok := spec.(*ast.StructType); ok {
		// Named structs are special in that its unexported fields are only used if they're being written to. That is,
		// the fields are not used by the named type itself, nor are the types of the fields.
		for _, field := range st.Fields.List {
			if len(field.Names) == 0 {
				fieldVar := g.embeddedField(field.Type)
				if hasExportedField(fieldVar.Type()) {
					// (6.5) structs use embedded structs that have exported fields (recursively)
					g.use(fieldVar, typ)
				}
			} else {
				for _, name := range field.Names {
					obj := g.pass.TypesInfo.ObjectOf(name)
					g.see(obj, typ)
					// (7.2) fields use their types
					g.read(field.Type, obj)
					if name.Name == "_" {
						g.use(obj, typ)
					} else if token.IsExported(name.Name) {
						// (6.2) structs use exported fields
						g.use(obj, typ)
					}

					if isNoCopyType(obj.Type()) {
						// (6.1) structs use fields of type NoCopy sentinel
						g.use(obj, typ)
					}
				}
			}

		}
	} else {
		g.read(spec, typ)
	}
}

func (g *graph) results() (used, unused []types.Object) {
	g.color(g.Root)

	var quieten func(n *node)
	quieten = func(n *node) {
		n.quiet = true
		for _, owned := range n.owns {
			quieten(owned)
		}
	}

	for _, n := range g.Nodes {
		if n.seen {
			continue
		}
		for _, owned := range n.owns {
			quieten(owned)
		}
	}

	// OPT(dh): can we find meaningful initial capacities for the used and unused slices?
	for _, n := range g.Nodes {
		switch obj := n.obj.(type) {
		case *types.Var:
			if obj.Name() == "" && obj.IsField() {
				// don't report unnamed variables (interface embedding)
				continue
			}
			if !obj.IsField() && obj.Parent() != obj.Pkg().Scope() {
				// Skip local variables, they're always used
				continue
			}
		}

		// XXX skip type parameters

		if n.obj.Pkg() != g.pass.Pkg {
			continue
		}
		if n.seen {
			used = append(used, n.obj)
		} else if !n.quiet {
			unused = append(unused, n.obj)
		}
	}

	return used, unused
}

func run(pass *analysis.Pass) (interface{}, error) {
	g := &graph{
		pass:  pass,
		Nodes: map[types.Object]*node{},
	}
	g.Root = g.newNode(nil)
	g.entry(pass)
	used, unused := g.results()

	// XXX don't flag methods in unused interfaces

	if true {
		// XXX make debug printing conditional
		debugNode := func(n *node) {
			if n.obj == nil {
				debugf("n%d [label=\"Root\"];\n", n.id)
			} else {
				color := "red"
				if n.seen {
					color = "green"
				} else if n.quiet {
					color = "grey"
				}
				debugf("n%d [label=%q, color=%q];\n", n.id, fmt.Sprintf("(%T) %s", n.obj, n.obj), color)
			}
			for _, e := range n.uses {
				debugf("n%d -> n%d;\n", n.id, e.id)
			}

			for _, owned := range n.owns {
				debugf("n%d -> n%d [style=dashed];\n", n.id, owned.id)
			}
		}

		debugf("digraph{\n")
		debugNode(g.Root)
		for _, v := range g.Nodes {
			debugNode(v)
		}

		debugf("}\n")
	}

	return Result{Used: used, Unused: unused}, nil
}

func assert(b bool) {
	if !b {
		panic("failed assertion")
	}
}
