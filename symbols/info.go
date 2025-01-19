package symbols

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
)

type Info struct {
	// Scopes maps any registered [ast.Node] to its *Scope providing
	// an easy lookup table for scope related information.
	//
	// The following nodes define their own Scope:
	// 	- *ast.FuncDeclaration
	//	- *ast.WhileStatement
	//	- *ast.IfStatement
	//	- *ast.Block
	//
	// In case of an [*ast.FuncDeclaration] the Scope
	// contains all parameters and local declarations.
	// In case of an [*ast.WhileStatement] and [*ast.IfStatement]
	// the Scope contains all declarations within the *ast.Block
	// of the statement.
	// In case of a [*ast.Block] the Scope contains all
	// declarations within that block.
	Scopes map[ast.Node]*Scope

	*traversalInfo
}

// DefinedBy returns the scope defined by the supplied [ast.Node] or
// nil if the [ast.Node] does not define a new Scope.
func (i Info) DefinedBy(n ast.Node) *Scope {
	s, ok := i.Scopes[n]
	if !ok {
		return nil
	}
	return s
}

// Find returns the [ast.Declaration] matching the supplied identifier
// along with the [Scope] it is defined in.
// If no ast.Declaration matches the identifier nil is returned
// for both the ast.Declaration and the Scope.
func (i Info) Find(identifier string) (*Scope, ast.Declaration) {
	if i.current == nil {
		i.traversalInfo.printStack()
		panic("current scope is nil")
	}
	return i.current.lookupClimb(identifier)
}

// FindInScope attempts to resolve the identifier in the current
// Scope. This function can only be used when also using Before and After
// to navigate scopes. Calling FindInScope must be called after calling
// Before at least once.
func (i Info) FindInScope(identifier string) ast.Declaration {
	if i.current == nil {
		i.traversalInfo.printStack()
		panic("current scope is nil")
	}
	_, decl := i.current.lookupClimb(identifier)
	return decl
}

type traversalInfo struct {
	current  *Scope
	triggers []ast.Node
}

func (tr *traversalInfo) printStack() {
	fmt.Printf("triggers:\n")
	for _, node := range tr.triggers {
		fmt.Printf("%T\n", node)
	}
}

func (i Info) Before(node ast.Node) {
	IfScopeRelevant(node, func(node ast.Node) {
		i.current = i.Scopes[node]
		// record who opened
		i.triggers = append(i.triggers, node)
	})
}

func (i Info) After(node ast.Node) {
	if i.current == nil {
		i.traversalInfo.printStack()
		panic("current scope is nil")
	}
	IfScopeRelevant(node, func(node ast.Node) {
		i.current = i.current.parent

		popped := i.triggers[len(i.triggers)-1]
		if popped != node {
			panic(fmt.Sprintf("node which closed %p is different from node which opened %p", node, popped))
		}

		i.triggers = i.triggers[:len(i.triggers)-1]
	})
}
