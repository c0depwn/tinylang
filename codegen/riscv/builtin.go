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

func provideBuiltInConvertToByte() builtInFunc {
	return builtInFunc{
		name: "byte",
		rewriteCall: func(expr *ast.CallExpression) {
			expr.Function.Name = internalToByte
		},
		generate: func(g *Generator) {
			g.asm.BeginProcedure(internalToByte)

			emitDefaultPrologue(g)
			defer emitDefaultEpilogue(g)

			g.asm.LoadImmediate(t0, 0b11111111)
			g.asm.And(a0, a0, t0)
		},
	}
}

func provideBuiltInConvertToString() builtInFunc {
	return builtInFunc{
		name: "string",
		rewriteCall: func(expr *ast.CallExpression) {
			expr.Function.Name = internalToString
		},
		generate: func(g *Generator) {
			g.asm.BeginProcedure(internalToString)

			emitDefaultPrologue(g)
			defer emitDefaultEpilogue(g)

			// TODO: improvement instead of creating empty/NOP internal funcs
			//       find a way to inline and or rewrite AST
		},
	}
}

func provideBuiltInConvertToInt() builtInFunc {
	return builtInFunc{
		name: "int",
		rewriteCall: func(expr *ast.CallExpression) {
			expr.Function.Name = internalToInt
		},
		generate: func(g *Generator) {
			g.asm.BeginProcedure(internalToInt)

			emitDefaultPrologue(g)
			defer emitDefaultEpilogue(g)

			g.asm.AddImmediate(a0, a0, 0)

			// TODO: improvement instead of creating empty/NOP internal funcs
			//       find a way to inline and or rewrite AST
		},
	}
}
