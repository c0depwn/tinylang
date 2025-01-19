package symbols

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
)

const (
	lookupErrFmt   = "could not resolve identifier '%s'"
	registerErrFmt = "'%s' redeclared"
)

type symbolPass struct {
	info    *Info
	current *Scope
}

func newSymbolPass() *symbolPass {
	return &symbolPass{
		info: &Info{
			Scopes:        make(map[ast.Node]*Scope),
			traversalInfo: &traversalInfo{},
		},
		current: nil,
	}
}

func (s *symbolPass) lookup(id *ast.Identifier) error {
	_, decl := s.current.lookupClimb(id.Name)
	if decl == nil {
		return fmt.Errorf(
			"%d:%d: %w",
			id.Position().Row,
			id.Position().Col,
			newSymbolErrorF(lookupErrFmt, id.Name),
		)
	}
	return nil
}

func (s *symbolPass) register(decl ast.Declaration) error {
	existing := s.current.register(decl)
	if existing != nil {
		return fmt.Errorf(
			"%d:%d: %w",
			decl.Position().Row,
			decl.Position().Col,
			newSymbolErrorF(registerErrFmt, decl.Name()),
		)
	}
	return nil
}

func (s *symbolPass) startScope(node ast.Node) {
	// assert pre-condition
	if node == nil {
		panic("logic error: symbolPass: node cannot be nil")
	}

	scope := newScope(s.current)
	s.info.Scopes[node] = scope
	s.current = scope
}

func (s *symbolPass) endScope() {
	// assert pre-condition
	if s.current == nil {
		panic("logic error: endScope called before startScope")
	}

	s.current = s.current.parent
}
