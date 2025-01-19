package ast

import (
	"fmt"
	"github.com/c0depwn/tinylang/token"
	"strings"
)

type Node interface {
	// Position returns the position of the first token
	// of the Node.
	Position() token.Position
	String() string

	// prevent external implementations
	aNode()
}

type node struct {
	p token.Position
}

func (n *node) SetPosition(p token.Position) { n.p = p }
func (n *node) Position() token.Position     { return n.p }
func (*node) aNode()                         {}

type File struct {
	node
	Declarations []Declaration
}

func (file *File) String() string {
	str := make([]string, len(file.Declarations))
	for i, d := range file.Declarations {
		str[i] = d.(fmt.Stringer).String()
	}
	return strings.Join(str, "\n")
}
