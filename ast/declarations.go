package ast

import (
	"fmt"
	"strings"
)

type Declaration interface {
	Node
	Name() string
	Type() Type
}

type declaration struct {
}

type VarDeclaration struct {
	statement
	declaration
	Identifier *Identifier
	TypeName   TypeIdentifier
	Expression Expression

	// The Type information is available after the type checking pass.
	T Type

	// The InitOrder is available after resolving dependencies between
	// variable initialization.
	InitOrder int
}

func (d *VarDeclaration) aSimpleStatement() {}

func (d *VarDeclaration) Name() string {
	return d.Identifier.Name
}

func (d *VarDeclaration) String() string {
	if d.Expression == nil {
		return fmt.Sprintf("var %s %s", d.Identifier, d.TypeName)
	}
	return fmt.Sprintf("var %s %s = %s", d.Identifier, d.TypeName, d.Expression)
}

func (d *VarDeclaration) Type() Type {
	return d.T
}

func (d *VarDeclaration) SetInitOrder(initOrder int) {
	d.InitOrder = initOrder
}

func (d *VarDeclaration) GetInitOrder() int {
	return d.InitOrder
}

type ConstDeclaration struct {
	statement
	declaration
	Identifier *Identifier
	TypeName   TypeIdentifier
	Expression ConstExpression

	T         Type
	InitOrder int
}

func (d *ConstDeclaration) Name() string {
	return d.Identifier.Name
}

func (d *ConstDeclaration) String() string {
	return fmt.Sprintf("const %s = %s;", d.Identifier, d.Expression)
}

func (d *ConstDeclaration) Type() Type {
	return d.T
}

func (d *ConstDeclaration) SetInitOrder(initOrder int) {
	d.InitOrder = initOrder
}

func (d *ConstDeclaration) GetInitOrder() int {
	return d.InitOrder
}

type FuncDeclaration struct {
	node
	declaration
	Identifier *Identifier
	Parameters []*Param
	Result     TypeIdentifier // TODO: support multiple results, requires multi var decl/assignment
	Body       *Block

	T Type
}

func (d *FuncDeclaration) Name() string {
	return d.Identifier.Name
}

func (d *FuncDeclaration) String() string {
	sb := &strings.Builder{}

	var temp []string
	for _, parameter := range d.Parameters {
		temp = append(temp, parameter.String())
	}

	if d.Result == nil {
		sb.WriteString(fmt.Sprintf(
			"fn %s(%s) ",
			d.Identifier,
			strings.Join(temp, ","),
		))
	} else {
		sb.WriteString(fmt.Sprintf(
			"fn %s(%s) %s ",
			d.Identifier,
			strings.Join(temp, ","),
			d.Result.String(),
		))
	}

	sb.WriteString(d.Body.String())

	return sb.String()
}

func (d *FuncDeclaration) Type() Type {
	return d.T
}

type Param struct {
	node
	declaration
	Identifier *Identifier
	TypeID     TypeIdentifier

	T Type
}

func (d *Param) Name() string {
	return d.Identifier.Name
}

func (d *Param) String() string {
	return fmt.Sprintf("%s %s", d.Identifier, d.TypeID)
}

func (d *Param) Type() Type {
	return d.T
}
