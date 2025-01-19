package ast

import (
	"fmt"
	"github.com/c0depwn/tinylang/pkg/slices"
	"strings"
)

type BasicLiteral struct {
	constExpression

	T Type
}

func (l *BasicLiteral) String() string {
	return l.Value().String()
}

func (l *BasicLiteral) Type() Type {
	return l.T
}

type ArrayLiteral struct {
	expression
	Elements []Expression
	TypeID   *ArrayType

	T Type
}

func (l *ArrayLiteral) Type() Type {
	return l.T
}

func (l *ArrayLiteral) String() string {
	strs := slices.Map(l.Elements, func(e Expression) string {
		return e.String()
	})
	return fmt.Sprintf("[%s]%s{%s}", l.TypeID.Len, l.TypeID.ElementType, strings.Join(strs, ","))
}
