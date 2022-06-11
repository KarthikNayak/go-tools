package unused

import (
	"fmt"
	"go/token"
	"go/types"

	"golang.org/x/tools/go/analysis"
	"honnef.co/go/tools/analysis/code"
	"honnef.co/go/tools/analysis/report"
)

type SerializedObject struct {
	Name            string
	Position        token.Position
	DisplayPosition token.Position
	Kind            string
	InGenerated     bool
}

type SerializedResult struct {
	Used   []SerializedObject
	Unused []SerializedObject
}

func Serialize(pass *analysis.Pass, res Result, fset *token.FileSet) SerializedResult {
	// OPT(dh): there's no point in serializing Used objects that are
	// always used, such as exported names, blank identifiers, or
	// anonymous struct fields. Used only exists to overrule Unused of
	// a different package. If something can never be unused, then its
	// presence in Used is useless.
	//
	// I'm not sure if this should happen when serializing, or when
	// returning Result.

	out := SerializedResult{
		Used:   make([]SerializedObject, len(res.Used)),
		Unused: make([]SerializedObject, len(res.Unused)),
	}
	for i, obj := range res.Used {
		out.Used[i] = serializeObject(pass, fset, obj)
	}
	for i, obj := range res.Unused {
		out.Unused[i] = serializeObject(pass, fset, obj)
	}
	return out
}

func typString(obj types.Object) string {
	switch obj := obj.(type) {
	case *types.Func:
		return "func"
	case *types.Var:
		if obj.IsField() {
			return "field"
		}
		return "var"
	case *types.Const:
		return "const"
	case *types.TypeName:
		if _, ok := obj.Type().(*types.TypeParam); ok {
			return "type param"
		} else {
			return "type"
		}
	default:
		return "identifier"
	}
}

func serializeObject(pass *analysis.Pass, fset *token.FileSet, obj types.Object) SerializedObject {
	name := obj.Name()
	if sig, ok := obj.Type().(*types.Signature); ok && sig.Recv() != nil {
		switch sig.Recv().Type().(type) {
		case *types.Named, *types.Pointer:
			typ := types.TypeString(sig.Recv().Type(), func(*types.Package) string { return "" })
			if len(typ) > 0 && typ[0] == '*' {
				name = fmt.Sprintf("(%s).%s", typ, obj.Name())
			} else if len(typ) > 0 {
				name = fmt.Sprintf("%s.%s", typ, obj.Name())
			}
		}
	}
	return SerializedObject{
		Name:            name,
		Position:        fset.PositionFor(obj.Pos(), false),
		DisplayPosition: report.DisplayPosition(fset, obj.Pos()),
		Kind:            typString(obj),
		InGenerated:     code.IsGenerated(pass, obj.Pos()),
	}
}
