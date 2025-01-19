package symbols

import (
	"github.com/c0depwn/tinylang/ast"
	"testing"
)

func TestScope_Declarations(t *testing.T) {
	foo := newVar("foo", newBasicT(""))
	bar := newVar("bar", newBasicT(""))

	// root <- parent <- child
	//			 |			|
	//			foo		   bar
	root := newScope(nil)
	parent := newScope(root)
	parent.register(foo)
	child := newScope(parent)
	child.register(bar)

	createAssertDeclarations()(t, root.Declarations())
	createAssertDeclarations(foo)(t, parent.Declarations())
	createAssertDeclarations(bar)(t, child.Declarations())
}

func TestScope_AllDeclarations(t *testing.T) {
	foo := newVar("foo", newBasicT(""))
	bar := newVar("bar", newBasicT(""))

	// root <- parent <- child
	//			 |			|
	//			foo		   bar
	root := newScope(nil)
	parent := newScope(root)
	parent.register(foo)
	child := newScope(parent)
	child.register(bar)

	createAssertDeclarations(foo, bar)(t, root.AllDeclarations())
	createAssertDeclarations(foo, bar)(t, parent.AllDeclarations())
	createAssertDeclarations(bar)(t, child.AllDeclarations())
}

func TestScope_Get(t *testing.T) {
	foo := newVar("foo", newBasicT(""))
	bar := newVar("bar", newBasicT(""))

	// root <- parent <- child
	//			 |			|
	//			foo		   bar
	root := newScope(nil)
	parent := newScope(root)
	parent.register(foo)
	child := newScope(parent)
	child.register(bar)

	// ensure found in correct scope
	createAssertGetResult(t, parent, foo)(child.Get(foo.Name()))
	createAssertGetResult(t, child, bar)(child.Get(bar.Name()))

	// ensure don't exist in root
	createAssertGetResult(t, nil, nil)(root.Get(foo.Name()))
	createAssertGetResult(t, nil, nil)(root.Get(bar.Name()))
}

func createAssertGetResult(
	t *testing.T,
	expectedScope *Scope,
	expectedDecl ast.Declaration,
) func(*Scope, ast.Declaration) {
	return func(scope *Scope, declaration ast.Declaration) {
		if expectedScope != scope {
			t.Fatalf("scope: expected '%v', got '%v'", expectedScope, scope)
		}
		if expectedDecl != declaration {
			t.Fatalf("declaration: expected '%v', got '%v'", expectedDecl, declaration)
		}
	}
}

func createAssertDeclarations(expected ...ast.Declaration) func(*testing.T, []ast.Declaration) {
	return func(t *testing.T, actual []ast.Declaration) {
		if len(expected) != len(actual) {
			t.Fatalf("unexpected number of declarations in scope: expected %d, got %d", len(expected), len(actual))
		}

		// the order is not guaranteed
		actualM := make(map[string]ast.Declaration)
		for _, decl := range actual {
			actualM[decl.Name()] = decl
		}

		for _, expectedDecl := range expected {
			actualDecl, ok := actualM[expectedDecl.Name()]
			if !ok {
				t.Fatalf("declaration not found in scope: %v", expectedDecl.Name())
			}
			if actualDecl != expectedDecl {
				t.Fatalf(
					"actual declaration '%s' does not match expected declaration: %v",
					actualDecl, expectedDecl,
				)
			}
		}
	}
}
