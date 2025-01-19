package semantics

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/symbols"
	"github.com/c0depwn/tinylang/types"
)

// Analyze ensures that the program adheres to the following rules:
//   - exactly 1 main func is present
//   - main is not called from within the program
//   - all non-void functions have at least one return
//   - ensure constants are not assigned to
func Analyze(f *ast.File, info symbols.Info) error {

	// TODO: improvement: generate a warning for call
	//       expressions which ignore a non-void function result

	if err := ensureMain(f, info); err != nil {
		return err
	}

	var traversal []ast.Node
	var err error
	var encounteredReturn bool

	ast.Inspect(f, func(n ast.Node) bool {
		if err != nil {
			return false
		}
		if n == nil {
			// pop
			popped := traversal[len(traversal)-1]
			traversal = traversal[:len(traversal)-1]
			info.After(popped) // TODO: not nice

			switch node := popped.(type) {
			case *ast.FuncDeclaration:
				funcType, ok := node.Type().(types.Function)
				if !ok {
					panic("not a function")
				}

				// dont care about void funcs
				if funcType.Result.Equals(types.NewVoid()) {
					return false
				}

				if !encounteredReturn {
					err = fmt.Errorf("missing return statement")
				} else {
					encounteredReturn = false
				}
			}

			return false
		}

		// push
		traversal = append(traversal, n)
		info.Before(n) // TODO: not nice

		switch node := n.(type) {
		case *ast.Assignment:
			err = ensureNoConstantAssignment(node, info)
		case *ast.CallExpression:
			err = ensureMainNotCalled(node)
		case *ast.ReturnStatement:
			encounteredReturn = true
		}

		return true
	})

	return err
}

func ensureMain(f *ast.File, info symbols.Info) error {
	for _, decl := range info.DefinedBy(f).Declarations() {
		fDecl, ok := decl.(*ast.FuncDeclaration)
		if !ok {
			continue
		}
		if fDecl.Name() != "main" {
			continue
		}
		return nil
	}

	return fmt.Errorf("missing main function")
}

func ensureMainNotCalled(expr *ast.CallExpression) error {
	if expr.Function.Name != "main" {
		return nil
	}
	return fmt.Errorf("cannot call main function")
}

func ensureNoConstantAssignment(expr *ast.Assignment, info symbols.Info) error {
	ident, ok := expr.Left.(*ast.Identifier)
	if !ok {
		return nil
	}

	decl := info.FindInScope(ident.Name)
	if decl == nil {
		panic("declaration must be present")
	}

	constDecl, ok := decl.(*ast.ConstDeclaration)
	if !ok {
		return nil
	}

	return fmt.Errorf(
		"%d:%d: cannot assign value to constant '%s'",
		ident.Position().Row,
		ident.Position().Col,
		constDecl.Name(),
	)
}
