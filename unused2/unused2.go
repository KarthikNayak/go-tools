package unused2

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"reflect"

	"golang.org/x/tools/go/analysis"
	"honnef.co/go/tools/analysis/facts/directives"
	"honnef.co/go/tools/analysis/facts/generated"
	"honnef.co/go/tools/analysis/lint"
	"honnef.co/go/tools/unused"
)

// XXX aliases/function vars are temporary and will vanish once we replace unused with unused2

type Result = unused.Result
type SerializedResult = unused.SerializedResult
type SerializedObject = unused.SerializedObject

var Serialize = unused.Serialize

// XXX all of these are only exported so we can refer to them in unused2, unexport them when we replace unused with
// unused2

type edgeKind = unused.EdgeKind

const (
	edgeAlias edgeKind = 1 << iota
	edgeBlankField
	edgeAnonymousStruct
	edgeCgoExported
	edgeConstGroup
	edgeElementType
	edgeEmbeddedInterface
	edgeExportedConstant
	edgeExportedField
	edgeExportedFunction
	edgeExportedMethod
	edgeExportedType
	edgeExportedVariable
	edgeExtendsExportedFields
	edgeExtendsExportedMethodSet
	edgeFieldAccess
	edgeFunctionArgument
	edgeFunctionResult
	edgeFunctionSignature
	edgeImplements
	edgeInstructionOperand
	edgeInterfaceCall
	edgeInterfaceMethod
	edgeKeyType
	edgeLinkname
	edgeMainFunction
	edgeInitFunction
	edgeNamedType
	edgeNetRPCRegister
	edgeNoCopySentinel
	edgeProvidesMethod
	edgeReceiver
	edgeRuntimeFunction
	edgeSignature
	edgeStructConversion
	edgeTestSink
	edgeTupleElement
	edgeType
	edgeTypeName
	edgeUnderlyingType
	edgePointerType
	edgeUnsafeConversion
	edgeUsedConstant
	edgeVarDecl
	edgeIgnored
	edgeSamePointer
	edgeTypeParam
	edgeTypeArg
	edgeUnionTerm
)

var typString = unused.TypString
var isIrrelevant = unused.IsIrrelevant
var isNoCopyType = unused.IsNoCopyType

var Analyzer = &lint.Analyzer{
	Doc: &lint.Documentation{
		Title: "Unused code",
	},
	Analyzer: &analysis.Analyzer{
		Name:       "U21000", // XXX change Name to U1000
		Doc:        "Unused code",
		Run:        run,
		Requires:   []*analysis.Analyzer{generated.Analyzer, directives.Analyzer},
		ResultType: reflect.TypeOf(Result{}),
	},
}

type node struct{}

var root types.Object = new(types.TypeName)
var rootExported types.Object = new(types.TypeName)

type graph struct {
}

func (g *graph) see(obj types.Object) {}

func (g *graph) use(used, by types.Object, kind edgeKind) {}

func (g *graph) seeAndUse() {}

func (g *graph) entry(pass *analysis.Pass) {
	info := pass.TypesInfo

	for _, f := range pass.Files {
		for _, decl := range f.Decls {
			switch decl := decl.(type) {
			case *ast.GenDecl:
				switch decl.Tok {
				case token.IMPORT:
					// Nothing to do
				case token.CONST:
					// XXX constants use their types
					// XXX process the rhs
				case token.TYPE:
					for _, spec := range decl.Specs {
						tspec := spec.(*ast.TypeSpec)
						g.see(info.ObjectOf(tspec.Name))
						if token.IsExported(tspec.Name.Name) {
							// (1.1) packages use exported named types
							g.use(info.ObjectOf(tspec.Name), rootExported, edgeExportedType)
						}

						// We can't just use all the idents making up this type. For example, for `type x struct { ...
						// }`, we don't want to use the struct fields.
						g.typ(info.TypeOf(tspec.Name))
					}
				case token.VAR:

					// XXX variables use their types
					// XXX process the rhs
				default:
					panic(fmt.Sprintf("unexpected token %s", decl.Tok))
				}
			case *ast.FuncDecl:
				obj := info.ObjectOf(decl.Name)
				if token.IsExported(decl.Name.Name) {
					if decl.Recv == nil {
						// (1.2) packages use exported functions
						g.use(obj, rootExported, edgeExportedFunction)
					} else {
						// (2.1) named types use exported methods
						// XXX implement
					}
				} else if decl.Name.Name == "init" {
					// (1.5) packages use init functions
					g.use(obj, root, edgeInitFunction)
				} else if decl.Name.Name == "main" && pass.Pkg.Name() == "main" {
					// (1.7) packages use the main function iff in the main package
					g.use(obj, root, edgeMainFunction)
				}

				// XXX implement most of 4.x here

				// We could just look at the idents in the function declaration, but we already have existing code for
				// handling types.Signature, which we need for type declarations.
				g.typ(info.TypeOf(decl.Type))

				g.funcBody(decl.Body, obj)
			default:
				// We do not cover BadDecl, but we shouldn't ever see one of those
				lint.ExhaustiveTypeSwitch(decl)
			}
		}
	}
}

// useNames resolves all ast.Idents reachable from node and marks them as used.
func (g *graph) useNames(node ast.Node, usedBy types.Object) {
	// XXX implement
}

func (g *graph) funcBody(body *ast.BlockStmt, usedBy types.Object) {
	// XXX is this sufficient?

	// (4.2) functions use anonymous functions defined beneath them
	// (4.3) functions use closures and bound methods
	// (4.4) functions use functions they return
	// (4.5) functions use functions/interface methods they call
	// (4.6) functions use types they instantiate or convert to
	// (4.7) functions use fields they access
	g.useNames(body, usedBy)

	// XXX what about 4.9? just using all the idents also uses all global variables that are read or assigned to,
	// regardless of if they're tests
}

func (g *graph) typ(typ types.Type) {
	// XXX skip types we've already processed
	// XXX implement most of 2.x here
	// XXX we can likely copy most if not all of unused.graph.typ
}

func run(pass *analysis.Pass) (interface{}, error) {
	g := &graph{}
	g.entry(pass)
	return nil, nil
}
