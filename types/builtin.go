package types

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
)

var builtInFuncs []builtInFunc

func byIdentifier(identifier *ast.Identifier) func(builtInFunc) bool {
	return func(builtin builtInFunc) bool {
		return builtin.identifier == identifier.Name
	}
}

type builtInFunc struct {
	identifier     string
	checkCall      func(args []ast.Expression) error
	provideResultT func() ast.Type
}

// provideBuiltInLen provides type checking for the built-in
// len(T) functions. len(T) accepts string or arrays as T
// and returns their length.
//
// For an Array the length is defined as the number of elements.
// For a String the length is defined as the number of characters.
//
// The type-specific implementation is generated during code-generation.
func provideBuiltInLen() builtInFunc {
	return builtInFunc{
		identifier: "len",
		checkCall: func(args []ast.Expression) error {
			if len(args) != 1 {
				return newTypeError("len expects 1 argument")
			}

			argT := args[0].Type()
			if IsPointer(argT) {
				argT = argT.Underlying()
			}

			switch argT.(type) {
			case Array:
			case String:
			default:
				return newTypeError("len expects array or string").WithActual(argT)
			}

			return nil
		},
		provideResultT: func() ast.Type { return NewInt() },
	}
}

// provideBuiltInPrint provides type checking for the built-in
// print(T) functions.
// The type-specific implementation is generated during code-generation.
// TODO: doc: provide a description of behaviour for each T
func provideBuiltInPrint() builtInFunc {
	return builtInFunc{
		identifier: "print",
		checkCall: func(args []ast.Expression) error {
			if len(args) != 1 {
				return newTypeError("len expects 1 arguments")
			}

			switch args[0].Type().(type) {
			// TODO: improvement: support Array
			case String:
			case Int:
			case Bool:
			case Pointer:
			default:
				return newTypeError(fmt.Sprintf("incompatible argument '%s' for print", args[0].Type()))
			}

			return nil
		},
		provideResultT: func() ast.Type { return NewVoid() },
	}
}

// provideBuiltInPrintLn provides type checking for the built-in
// println(T) functions.
// The type-specific implementation is generated during code-generation.
// TODO: doc: provide a description of behaviour for each T
func provideBuiltInPrintLn() builtInFunc {
	return builtInFunc{
		identifier: "println",
		checkCall: func(args []ast.Expression) error {
			if len(args) != 1 {
				return newTypeError("len expects 1 arguments")
			}

			switch args[0].Type().(type) {
			// TODO: improvement: support Array
			case String:
			case Int:
			case Bool:
			case Pointer:
			default:
				return newTypeError(fmt.Sprintf("incompatible argument '%s' for print", args[0].Type()))
			}

			return nil
		},
		provideResultT: func() ast.Type { return NewVoid() },
	}
}
