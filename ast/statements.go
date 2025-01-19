package ast

import (
	"fmt"
	"github.com/c0depwn/tinylang/pkg/slices"
	"strings"
)

type Statement interface {
	Node
	// Prevent external implementation
	aStatement()
}

type statement struct{ node }

func (statement) aStatement() {}

type SimpleStatement interface {
	Statement
	// Prevent external implementation
	aSimpleStatement()
}

type simpleStatement struct{ statement }

func (simpleStatement) aSimpleStatement() {}

type Block struct {
	statement
	Statements []Statement
}

func (b *Block) String() string {
	sb := &strings.Builder{}

	strs := slices.Map(b.Statements, func(s Statement) string {
		return s.String()
	})

	sb.WriteString("{\n")
	sb.WriteString(strings.Join(strs, ";\n"))
	if len(strs) > 0 {
		sb.WriteString(";\n")
	}
	sb.WriteString("}")

	return sb.String()
}

type Assignment struct {
	simpleStatement
	Left  Expression
	Value Expression
}

func (a *Assignment) String() string {
	return fmt.Sprintf("%s = %s", a.Left, a.Value)
}

type IfStatement struct {
	statement
	Condition   Expression
	Consequence *Block
	Alternative Statement
}

func (i *IfStatement) String() string {
	str := fmt.Sprintf("if %s %s ", i.Condition, i.Consequence)
	if i.Alternative != nil {
		str += fmt.Sprintf("else %s", i.Alternative)
	}
	return str
}

type WhileStatement struct {
	statement
	Condition Expression
	Body      *Block
}

func (w *WhileStatement) String() string {
	return fmt.Sprintf("while %s %s", w.Condition, w.Body.String())
}

type ForStatement struct {
	statement
	Pre       SimpleStatement
	Condition Expression
	Post      SimpleStatement // TODO: allow inc/dec with x++ or x--
	Body      *Block
}

func (f *ForStatement) String() string {
	str := "for "
	if f.Pre != nil {
		str += f.Pre.(fmt.Stringer).String()
	}
	str += "; "
	if f.Condition != nil {
		str += f.Condition.(fmt.Stringer).String()
	}
	str += "; "
	if f.Post != nil {
		str += f.Post.(fmt.Stringer).String()
	}
	str += f.Body.String()
	return str
}

type ReturnStatement struct {
	statement
	Expression Expression
}

func (r *ReturnStatement) String() string {
	return fmt.Sprintf("return %s", r.Expression)
}

type ExpressionStatement struct {
	statement
	Expression Expression
}

func (e *ExpressionStatement) String() string {
	return fmt.Sprintf("%s", e.Expression)
}
