package riscv

import (
	"github.com/c0depwn/tinylang/ast"
	"slices"
)

var builtInFuncs []builtInFunc

func findBuiltIn(fn string) (builtInFunc, bool) {
	idx := slices.IndexFunc(builtInFuncs, func(inFunc builtInFunc) bool {
		return inFunc.name == fn
	})
	if idx >= 0 {
		return builtInFuncs[idx], true
	}
	return builtInFunc{}, false
}

func registerAndGenerateBuiltIn(
	g *Generator,
	inFuncs ...builtInFunc,
) {
	for i := range inFuncs {
		// generate code for built-in
		inFuncs[i].generate(g)
		// add to builtInFuncs
		builtInFuncs = append(builtInFuncs, inFuncs[i])
	}
}

type builtInFunc struct {
	name string
	// rewriteCall allows mutating the call to the built-in function.
	// E.g. print(someInt) -> print_int(someInt)
	rewriteCall func(expr *ast.CallExpression)
	// generate is a hook invoked to generate the code of the function.
	generate func(g *Generator)
}

func provideBuiltInLen() builtInFunc {
	return builtInFunc{
		name:        "len",
		rewriteCall: func(expr *ast.CallExpression) {},
		generate: func(g *Generator) {
			g.asm.BeginProcedure("len")
			emitDefaultPrologue(g)
			g.asm.LoadFromOffset(a0, a0, 0)
			emitDefaultEpilogue(g)
		},
	}
}
