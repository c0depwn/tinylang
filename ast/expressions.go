package ast

import (
	"fmt"
	"github.com/c0depwn/tinylang/constant"
	"strings"
)

type Expression interface {
	Node
	Type() Type
	aExpression()
}

type expression struct {
	node
}

func (expression) aExpression() {}

type ConstExpression interface {
	Expression
	Value() constant.Value
	aConstExpression()
}

type constExpression struct {
	expression
	v constant.Value
}

func (ce *constExpression) Value() constant.Value {
	return ce.v
}

func (ce *constExpression) SetValue(v constant.Value) {
	ce.v = v
}

func (ce *constExpression) aConstExpression() {}

// Identifier [a-zA-Z_].+[a-zA-Z0-9_].*
type Identifier struct {
	expression
	Name string

	T Type
}

func (i *Identifier) String() string {
	return i.Name
}

func (i *Identifier) Type() Type { return i.T }

// InfixExpression = Left Operator Right
type InfixExpression struct {
	expression
	Operator    string
	Left, Right Expression

	T Type
}

func (ie *InfixExpression) String() string {
	return fmt.Sprintf("(%s %s %s)", ie.Left, ie.Operator, ie.Right)
}

func (ie *InfixExpression) Type() Type { return ie.T }

// PrefixExpression = Operator(Right)
type PrefixExpression struct {
	expression
	Operator string
	Right    Expression

	T Type
}

func (pe *PrefixExpression) String() string {
	return fmt.Sprintf("%s(%s)", pe.Operator, pe.Right)
}

func (pe *PrefixExpression) Type() Type { return pe.T }

// IndexExpression = Left[Index]
type IndexExpression struct {
	expression
	Left  Expression
	Index Expression

	T Type
}

func (ie *IndexExpression) String() string {
	return fmt.Sprintf("%s[%s]", ie.Left, ie.Index)
}

func (ie *IndexExpression) Type() Type { return ie.T }

// CallExpression = Function(Arguments...)
// TODO: The Function must be an Expression as soon as treating	functions as variables is supported.
type CallExpression struct {
	expression
	Function  *Identifier
	Arguments []Expression

	T Type
}

func (ce *CallExpression) String() string {
	strs := make([]string, len(ce.Arguments))
	for i := range ce.Arguments {
		strs[i] = ce.Arguments[i].String()
	}
	return fmt.Sprintf(
		"%s(%s)",
		ce.Function,
		strings.Join(strs, ","),
	)
}

func (ce *CallExpression) Type() Type { return ce.T }
