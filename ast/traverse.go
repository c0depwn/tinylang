package ast

import "fmt"

// This is inspired by the go implementation of AST traversal.
// https://go.dev/src/go/ast/walk.go

type Visitor interface {
	Visit(node Node) Visitor
}

func Inspect(root Node, f func(Node) bool) {
	Walk(root, inspector(f))
}

type inspector func(Node) bool

func (v inspector) Visit(node Node) Visitor {
	if v(node) {
		return v
	}
	return nil
}

func Walk(root Node, v Visitor) {
	walker{v: v}.walk(root)
}

type walker struct {
	v Visitor
}

func (w walker) walk(n Node) {
	if n == nil {
		panic("walk received nil node")
	}

	w.v = w.v.Visit(n)
	if w.v == nil {
		return
	}

	switch node := n.(type) {
	case *File:
		for _, d := range node.Declarations {
			w.walk(d)
		}

	// Statements

	case *Assignment:
		w.walk(node.Left)
		w.walk(node.Value)

	case *Block:
		for _, stmt := range node.Statements {
			w.walk(stmt)
		}

	case *IfStatement:
		w.walk(node.Condition)
		w.walk(node.Consequence)
		if node.Alternative != nil {
			w.walk(node.Alternative)
		}

	case *WhileStatement:
		w.walk(node.Condition)
		w.walk(node.Body)

	case *ReturnStatement:
		if node.Expression != nil {
			w.walk(node.Expression)
		}

	case *ExpressionStatement:
		w.walk(node.Expression)

	// Declarations
	case *VarDeclaration:
		// traversing variable identifier & type name might be useless
		// w.walk(node.Identifier)
		// w.walk(node.TypeName)
		if node.Expression != nil {
			w.walk(node.Expression)
		}

	case *ConstDeclaration:
		// traversing variable identifier & type name might be useless
		// w.walk(node.Identifier)
		// w.walk(node.TypeName)
		w.walk(node.Expression)

	case *FuncDeclaration:
		w.walk(node.Identifier)
		for _, param := range node.Parameters {
			w.walk(param)
		}
		w.walk(node.Body)
		if node.Result != nil {
			w.walk(node.Result)
		}

	case *Param: // leaf

	// Expressions

	case *Identifier: // leaf

	case *BasicTypeName: // leaf

	case *ArrayType:
		w.walk(node.Len)
		w.walk(node.ElementType)

	case *BasicLiteral: // leaf

	case *ArrayLiteral:
		for _, element := range node.Elements {
			w.walk(element)
		}

	case *IndexExpression:
		w.walk(node.Left)
		w.walk(node.Index)

	case *PrefixExpression:
		w.walk(node.Right)

	case *InfixExpression:
		w.walk(node.Left)
		w.walk(node.Right)

	case *CallExpression:
		w.walk(node.Function)
		for _, argument := range node.Arguments {
			w.walk(argument)
		}
	default:
		panic(fmt.Errorf("unhandled node type in walker: %T", node))
	}

	w.v.Visit(nil)
}
