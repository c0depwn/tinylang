package symbols

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
)

// IsGlobal reports whether the supplied Scope
// is the outermost Scope.
func IsGlobal(scope *Scope) bool {
	return scope.parent == nil
}

// Scope represents a declaration scope. Any [ast.Node] implementing [ast.Declaration]
// is registered into the appropriate Scope while traversing the AST in Checker.
type Scope struct {
	id           int
	parent       *Scope
	children     []*Scope
	declarations map[string]ast.Declaration
}

func newScope(parent *Scope) *Scope {
	s := new(Scope)
	s.declarations = make(map[string]ast.Declaration)
	s.parent = parent

	if parent == nil {
		s.id = 0
	} else {
		s.id = parent.id + 1
		parent.children = append(parent.children, s)
	}

	return s
}

// Get returns the [ast.Declaration] with the supplied name.
// If no [ast.Declaration] is found in the current Scope or
// any of its ancestors, nil is returned.
func (s *Scope) Get(name string) (*Scope, ast.Declaration) {
	scope, decl := s.lookupClimb(name)
	return scope, decl
}

// MustGet wraps Get and panics if no [ast.Declaration] is found.
func (s *Scope) MustGet(name string) (*Scope, ast.Declaration) {
	if scope, decl := s.Get(name); decl != nil {
		return scope, decl
	}
	panic(fmt.Errorf("missing entity for parameter %s", name))
}

// Declarations retrieves all [ast.Declaration] in the current Scope.
func (s *Scope) Declarations() []ast.Declaration {
	declarations := make([]ast.Declaration, 0)
	for _, d := range s.declarations {
		declarations = append(declarations, d)
	}
	return declarations
}

// AllDeclarations retrieves all [ast.Declaration] in the current Scope and
// all of its children.
func (s *Scope) AllDeclarations() []ast.Declaration {
	return recFind(s)
}

// recFind recursively discovers and returns all declarations
// in the supplied Scope. It is the backend of AllDeclarations.
func recFind(s *Scope) []ast.Declaration {
	decls := make([]ast.Declaration, 0)
	for _, decl := range s.declarations {
		decls = append(decls, decl)
	}

	if len(s.children) == 0 {
		return decls
	}

	for _, childScope := range s.children {
		decls = append(decls, recFind(childScope)...)
	}

	return decls
}

// lookup checks if the name is defined in the current Scope.
// If the name is not found, nil is returned.
func (s *Scope) lookup(name string) ast.Declaration {
	if decl, ok := s.declarations[name]; ok {
		return decl
	}
	return nil
}

// lookupClimb checks if the supplied name is defined in the current Scope
// or any of its ancestors.
// Either the Scope in which the Entity is found and the Entity itself is returned
// or nil for both values if the identifier could not be found.
func (s *Scope) lookupClimb(name string) (*Scope, ast.Declaration) {
	current := s
	for current != nil {
		if decl := current.lookup(name); decl != nil {
			return current, decl
		}
		current = current.parent
	}
	return nil, nil
}

// register the given Entity in the current Scope.
// If the identifier already exists within this or any parent Scope,
// the existing Entity is returned.
func (s *Scope) register(decl ast.Declaration) ast.Declaration {
	if _, existingDecl := s.lookupClimb(decl.Name()); existingDecl != nil {
		return existingDecl
	}
	s.declarations[decl.Name()] = decl
	return nil
}
