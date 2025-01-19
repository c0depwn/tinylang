package symbols

import (
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/token"
	"reflect"
	"slices"
)

// Analyze all symbols contained within the [*ast.File].
// The returned Info contains all declaration enclosed within
// their Scope.
//
// Analyze will terminate on the first error it encounters.
//
// Any [ast.Declaration] declares a "symbol" which can be referenced
// throughout the program. Some rules apply depending on the
// specific type of [ast.Declaration].
// Any [*ast.VarDeclaration] or [*ast.ConstDeclaration] can
// be defined in a global or local Scope.
// [*ast.FuncDeclaration] can only appear in the global Scope.
//
// TODO: improvement: add debug and tracing capabilities
func Analyze(f *ast.File, options ...AnalyzeOption) (Info, error) {

	// initialize pass
	pass := newSymbolPass()

	// global scope
	pass.startScope(f)
	defer pass.endScope()

	// apply custom options
	for _, opt := range options {
		opt(pass)
	}

	// Register global symbol definitions (var, const, fn)
	// This is done first so that the order of the definition does not
	// matter. For example, a function can be called by another function
	// even if it is declared after the calling function.
	// Another example: In the global scope, a variable can be used in
	// another variables initialization expression, independently of
	// their defined order.
	if err := registerGlobals(pass, f); err != nil {
		return Info{}, err
	}

	// Traverse parameters and body of function declarations.
	if err := registerLocals(pass, f); err != nil {
		return Info{}, err
	}

	return *pass.info, nil
}

func registerGlobals(pass *symbolPass, file *ast.File) error {
	var err error

	ast.Inspect(file, func(n ast.Node) bool {
		if err != nil {
			return false
		}
		if n == nil {
			return true
		}

		// Register global declarations and signal that
		// further exploration of the child nodes is not
		// desired by returning false.
		switch node := n.(type) {
		case *ast.VarDeclaration:
			err = pass.register(node)
			return false
		case *ast.ConstDeclaration:
			err = pass.register(node)
			return false
		case *ast.FuncDeclaration:
			err = pass.register(node)
			return false
		}

		return true
	})

	return err
}

func registerLocals(pass *symbolPass, file *ast.File) error {
	var (
		nodeStack []ast.Node
		err       error
	)

	for _, decl := range file.Declarations {

		// skip global declarations
		switch decl.(type) {
		case *ast.VarDeclaration:
			continue
		case *ast.ConstDeclaration:
			continue
		}

		// Traverse parameters and body of function declarations.
		ast.Inspect(decl, func(n ast.Node) bool {
			// signal early termination on error
			if err != nil {
				return false
			}

			// post-traversal
			if n == nil {
				poppedNode := nodeStack[len(nodeStack)-1]
				nodeStack = nodeStack[:len(nodeStack)-1]

				// Local declarations are registered post-traversal
				// this ensures that declarations cannot be used within
				// their own expression e.g. var x int = f(x); is illegal.
				switch node := poppedNode.(type) {
				case *ast.VarDeclaration:
					// avoid re-registering variables from the global scope
					if !IsGlobal(pass.current) {
						err = pass.register(node)
					}
				case *ast.ConstDeclaration:
					// avoid re-registering constants from the global scope
					if !IsGlobal(pass.current) {
						err = pass.register(node)
					}
				}

				// close the scope for nodes which opened a new scope
				IfScopeRelevant(poppedNode, func(_ ast.Node) { pass.endScope() })
				return true
			}

			// add to traversal stack
			nodeStack = append(nodeStack, n)
			// open a scope for nodes which define a new scope
			IfScopeRelevant(n, func(node ast.Node) { pass.startScope(node) })

			switch node := n.(type) {
			case *ast.Param:
				err = pass.register(node)
			case *ast.Identifier:
				if slices.Contains(builtInIdentifiers, node.Name) {
					return true
				}
				if token.IsReservedKeyword(node.Name) {
					return true
				}

				// Try to resolve encountered identifiers.
				// This ensures that no unknown identifiers are used
				err = pass.lookup(node)
			}

			return true
		})
	}

	return err
}

var scopeRelevantNodeTypes = []reflect.Type{
	reflect.TypeOf(&ast.File{}),
	reflect.TypeOf(&ast.Block{}),
	reflect.TypeOf(&ast.FuncDeclaration{}),
}

func IfScopeRelevant(node ast.Node, f func(ast.Node)) {
	isRelevant := slices.Contains(scopeRelevantNodeTypes, reflect.TypeOf(node))
	if !isRelevant {
		return
	}
	f(node)
}
