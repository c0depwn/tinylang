package riscv

import (
	"fmt"
	"strings"
)

type dataEmitDirective string

const (
	emitString = ".string"
	emitWord   = ".word"
)

type emitData struct {
	kind  dataEmitDirective
	value string
}

// pseudoASM provides generic low-level instructions.
// This abstraction is used to avoid having to know all exact
// details of the underlying architecture during code-generations.
// It is meant to generalize fine-grained instructions and hide quirks
// of the underlying non-pseudo instructions.
// The implementation of this interface must take care of handling
// special cases and details of the underlying architecture.
//
// TODO: improvement: this abstraction is not great. All instructions should be available
//
//	as compilation for 64 bits should also be able to instructions
//	which operate on smaller values than 8 bytes. Currently, this suffices
//	but it was a mistake to implement it like this. Refactor when possible.
type pseudoASM interface {
	DebugAddComment(msg string)
	AddDirective(dir string)
	// data section related operations

	// DefineData allows defining a global read-write variable.
	DefineData(label string, data emitData)
	// LoadDataAddress loads the address of the given label into the dst Register.
	LoadDataAddress(label string, dst Register)

	// text section related operations

	BeginProcedure(label string)
	EmitRawProcedure(label string, content string)

	// Move values from register rs1 into register rd.
	Move(rd, rs1 Register)
	// StoreAtOffset stores the value in register "from"
	// to the address base + offset.
	StoreAtOffset(src, base Register, offset int)
	// LoadImmediate loads the value into the dst Register.
	LoadImmediate(dst Register, value int)
	// LoadFromOffset loads the value at base + offset
	// into register "dst".
	LoadFromOffset(dst, base Register, offset int)
	// control flow

	// BranchEQZ will take the branch if the specified register
	// is equal to 0.
	BranchEQZ(label string, a Register)
	// Jump to the specified label.
	Jump(label string)
	// Insert a label at the current position.
	Insert(label string)
	// Call emits a procedure-call. The underlying implementation of call
	// is up to the implementation of pseudoASM with the following constraint:
	// Calls must be made in a position independent fashion.
	Call(label string)
	// SysCall performs an environment call using the ecall instruction
	SysCall()
	// Return emits a return from a sub-routine.
	Return()

	// stack related operations

	// Push the contents of the Register onto the stack.
	Push(src Register)
	// Pop the top of the stack into the dst Register.
	Pop(dst Register)

	// ops for two operands OP(a, b)

	AddImmediate(rd, rs1 Register, imm int)
	Add(dst, a, b Register)
	Sub(dst, a, b Register)
	Mul(dst, a, b Register)
	Div(dst, a, b Register)
	LessThan(dst, a, b Register)
	LessThanOrEqual(dst, a, b Register)
	GreaterThan(dst, a, b Register)
	GreaterThanOrEqual(dst, a, b Register)

	// ops for a single operand OP(a)

	Neg(dst, a Register)

	String() string
}

type textSection struct {
	directives []string
	procedures map[string][]string
}

func (s textSection) append(label, code string) {
	s.procedures[label] = append(s.procedures[label], code)
}

type dataSection struct {
	variables map[string]emitData
}

func (s dataSection) emit(label string, data emitData) {
	s.variables[label] = data
}

// enforce interface implementation

var _ pseudoASM = (*rv32PseudoASMImpl)(nil)
var _ pseudoASM = (*rv64PseudoASMImpl)(nil)

type commonRV struct {
	data         dataSection
	text         textSection
	currentLabel string
}

// emit instruction into current procedure
func (p *commonRV) emit(s string) {
	p.text.append(p.currentLabel, s)
}

func (p *commonRV) DebugAddComment(msg string) {
	p.text.append(p.currentLabel, fmt.Sprintf("#%s", msg))
}

func (p *commonRV) AddDirective(dir string) {
	p.text.directives = append(p.text.directives, dir)
}

func (p *commonRV) DefineData(l string, data emitData) {
	p.data.emit(l, data)
}

func (p *commonRV) BeginProcedure(label string) {
	p.currentLabel = label
}

func (p *commonRV) EmitRawProcedure(label string, content string) {
	p.text.procedures[label] = append(p.text.procedures[label], strings.Split(content, "\n")...)
}

func (p *commonRV) Call(label string) {
	// jump-and-link jumps to the offset
	// and stores the return address in the ra register.
	p.emit(fmt.Sprintf("jal ra, %s", label))
}

func (p *commonRV) SysCall() {
	p.emit("ecall")
}

func (p *commonRV) Return() {
	// jump to address in ra register
	p.emit("ret")
}

func (p *commonRV) Insert(label string) {
	p.emit(fmt.Sprintf("%s:", label))
}

func (p *commonRV) String() string {
	sb := &strings.Builder{}

	if len(p.data.variables) > 0 {
		sb.WriteString(".data\n")
		for label, data := range p.data.variables {
			if data.kind == emitString {
				sb.WriteString(fmt.Sprintf("%s: %s \"%s\"\n", label, data.kind, data.value))
			} else {
				sb.WriteString(fmt.Sprintf("%s: %s %s\n", label, data.kind, data.value))
			}
		}
	}

	sb.WriteString("\n.text\n")

	// add additional directives
	sb.WriteString(strings.Join(p.text.directives, "\n"))

	// TODO: improvement: this is target specific and should not be done here
	if _, exists := p.text.procedures["_start"]; exists {
		// _start is always first, if it exists
		sb.WriteString(fmt.Sprintf("\n_start:\n  "))
		sb.WriteString(strings.Join(p.text.procedures["_start"], "\n  "))
	}

	for label, code := range p.text.procedures {
		// TODO: improvement: having to check for this here is ugly
		if label == "_start" {
			continue
		}
		sb.WriteString(fmt.Sprintf("\n%s:\n  ", label))
		sb.WriteString(strings.Join(code, "\n  "))
	}

	return sb.String()
}

type rv32PseudoASMImpl struct {
	*commonRV
}

func newPseudoASM32Impl() *rv32PseudoASMImpl {
	return &rv32PseudoASMImpl{
		commonRV: &commonRV{
			data:         dataSection{variables: map[string]emitData{}},
			text:         textSection{procedures: map[string][]string{}},
			currentLabel: "",
		},
	}
}

func (p *rv32PseudoASMImpl) LoadDataAddress(label string, dst Register) {
	p.emit(fmt.Sprintf("la %s, %s", dst, label))
}

func (p *rv32PseudoASMImpl) Move(rd, rs1 Register) {
	p.emit(fmt.Sprintf("addi %s, %s, 0", rd, rs1))
}

func (p *rv32PseudoASMImpl) StoreAtOffset(src, base Register, offset int) {
	p.emit(fmt.Sprintf("sw %s, %d(%s)", src, offset, base))
}

func (p *rv32PseudoASMImpl) LoadImmediate(dst Register, value int) {
	// TODO: bug: loading using load immediate is probably broken if the
	//       value overflows a 12 bits (signed) int
	p.emit(fmt.Sprintf("li %s, %d", dst, value))
}

func (p *rv32PseudoASMImpl) LoadFromOffset(dst, base Register, offset int) {
	p.emit(fmt.Sprintf("lw %s, %d(%s) ", dst, offset, base))
}

func (p *rv32PseudoASMImpl) BranchEQZ(label string, a Register) {
	p.emit(fmt.Sprintf("beq %s, x0, %s", a, label))
}

func (p *rv32PseudoASMImpl) Jump(label string) {
	// JAL puts the return address in x0, which causes
	// it to be ignored as x0 is wired to 0.
	// This is fine as we don't care about the return
	// address in this case.
	p.emit(fmt.Sprintf("jal x0, %s", label))
}

func (p *rv32PseudoASMImpl) Push(src Register) {
	// p.DebugAddComment("push")
	p.emit(fmt.Sprintf("addi sp, sp, -16"))
	p.emit(fmt.Sprintf("sw %s, 0(sp)", src))
}

func (p *rv32PseudoASMImpl) Pop(dst Register) {
	// p.DebugAddComment("pop")
	p.emit(fmt.Sprintf("lw %s, 0(sp)", dst))
	p.emit(fmt.Sprintf("addi sp, sp, 16"))
}

func (p *rv32PseudoASMImpl) AddImmediate(rd, rs1 Register, imm int) {
	p.emit(fmt.Sprintf("addi %s, %s, %d", rd, rs1, imm))
}

func (p *rv32PseudoASMImpl) Add(dst, a, b Register) {
	p.emit(fmt.Sprintf("add %s, %s, %s", dst, a, b))
}

func (p *rv32PseudoASMImpl) Sub(dst, a, b Register) {
	p.emit(fmt.Sprintf("sub %s, %s, %s", dst, a, b))
}

func (p *rv32PseudoASMImpl) Mul(dst, a, b Register) {
	p.emit(fmt.Sprintf("mul %s, %s, %s", dst, a, b))
}

func (p *rv32PseudoASMImpl) Div(dst, a, b Register) {
	p.emit(fmt.Sprintf("div %s, %s, %s", dst, a, b))
}

func (p *rv32PseudoASMImpl) LessThan(dst, a, b Register) {
	p.emit(fmt.Sprintf("slt %s, %s, %s", dst, a, b))
}

func (p *rv32PseudoASMImpl) LessThanOrEqual(dst, a, b Register) {
	// a <= b is equivalent to !(b < a)

	// b < a
	p.emit(fmt.Sprintf("slt %s, %s, %s", dst, b, a))
	// not using XOR
	p.emit(fmt.Sprintf("xori %s, %s, 1", dst, dst))
}

func (p *rv32PseudoASMImpl) GreaterThan(dst, a, b Register) {
	// less than flipped e.g. a > b <=> b < a
	p.emit(fmt.Sprintf("slt %s, %s, %s", dst, b, a))
}

func (p *rv32PseudoASMImpl) GreaterThanOrEqual(dst, a, b Register) {
	// a >= b is equivalent to !(a < b)

	// a < b
	p.emit(fmt.Sprintf("slt %s, %s, %s", dst, a, b))
	// not using XOR
	p.emit(fmt.Sprintf("xori %s, %s, 1", dst, dst))
}

func (p *rv32PseudoASMImpl) Neg(dst, a Register) {
	p.emit(fmt.Sprintf("sub %s, x0, %s", dst, a))
}

type rv64PseudoASMImpl struct {
	*commonRV
}

func newPseudoASM64Impl() *rv64PseudoASMImpl {
	return &rv64PseudoASMImpl{
		commonRV: &commonRV{
			data:         dataSection{variables: map[string]emitData{}},
			text:         textSection{procedures: map[string][]string{}},
			currentLabel: "",
		},
	}
}

func (p *rv64PseudoASMImpl) LoadDataAddress(label string, dst Register) {
	p.emit(fmt.Sprintf("la %s, %s", dst, label))
}

func (p *rv64PseudoASMImpl) Move(rd, rs1 Register) {
	// p.emit(fmt.Sprintf("addi %s, %s, 0", rd, rs1))
	p.emit(fmt.Sprintf("mv %s, %s", rd, rs1))
}

func (p *rv64PseudoASMImpl) StoreAtOffset(src, base Register, offset int) {
	p.emit(fmt.Sprintf("sd %s, %d(%s)", src, offset, base))
}

func (p *rv64PseudoASMImpl) LoadImmediate(dst Register, value int) {
	// TODO: bug: loading using load immediate is probably broken if the
	//       value overflows a 12 bits (signed) int
	p.emit(fmt.Sprintf("li %s, %d", dst, value))
}

func (p *rv64PseudoASMImpl) LoadFromOffset(dst, base Register, offset int) {
	p.emit(fmt.Sprintf("ld %s, %d(%s) ", dst, offset, base))
}

func (p *rv64PseudoASMImpl) BranchEQZ(label string, a Register) {
	p.emit(fmt.Sprintf("beq %s, x0, %s", a, label))
}

func (p *rv64PseudoASMImpl) Jump(label string) {
	// JAL puts the return address in x0, which causes
	// it to be ignored as x0 is wired to 0.
	// This is fine as we don't care about the return
	// address in this case.
	p.emit(fmt.Sprintf("jal x0, %s", label))
}

func (p *rv64PseudoASMImpl) Push(src Register) {
	p.emit(fmt.Sprintf("addi sp, sp, -16"))
	p.emit(fmt.Sprintf("sd %s, 0(sp)", src))
}

func (p *rv64PseudoASMImpl) Pop(dst Register) {
	p.emit(fmt.Sprintf("ld %s, 0(sp)", dst))
	p.emit(fmt.Sprintf("addi sp, sp, 16"))
}

func (p *rv64PseudoASMImpl) AddImmediate(rd, rs1 Register, imm int) {
	p.emit(fmt.Sprintf("addi %s, %s, %d", rd, rs1, imm))
}

func (p *rv64PseudoASMImpl) Add(dst, a, b Register) {
	p.emit(fmt.Sprintf("add %s, %s, %s", dst, a, b))
}

func (p *rv64PseudoASMImpl) Sub(dst, a, b Register) {
	p.emit(fmt.Sprintf("sub %s, %s, %s", dst, a, b))
}

func (p *rv64PseudoASMImpl) Mul(dst, a, b Register) {
	p.emit(fmt.Sprintf("mul %s, %s, %s", dst, a, b))
}

func (p *rv64PseudoASMImpl) Div(dst, a, b Register) {
	p.emit(fmt.Sprintf("div %s, %s, %s", dst, a, b))
}

func (p *rv64PseudoASMImpl) LessThan(dst, a, b Register) {
	p.emit(fmt.Sprintf("slt %s, %s, %s", dst, a, b))
}

func (p *rv64PseudoASMImpl) LessThanOrEqual(dst, a, b Register) {
	// a <= b is equivalent to !(b < a)

	// b < a
	p.emit(fmt.Sprintf("slt %s, %s, %s", dst, b, a))
	// not using XOR
	p.emit(fmt.Sprintf("xori %s, %s, 1", dst, dst))
}

func (p *rv64PseudoASMImpl) GreaterThan(dst, a, b Register) {
	// less than flipped e.g. a > b <=> b < a
	p.emit(fmt.Sprintf("slt %s, %s, %s", dst, b, a))
}

func (p *rv64PseudoASMImpl) GreaterThanOrEqual(dst, a, b Register) {
	// a >= b is equivalent to !(a < b)

	// a < b
	p.emit(fmt.Sprintf("slt %s, %s, %s", dst, a, b))
	// not using XOR
	p.emit(fmt.Sprintf("xori %s, %s, 1", dst, dst))
}

func (p *rv64PseudoASMImpl) Neg(dst, a Register) {
	p.emit(fmt.Sprintf("sub %s, x0, %s", dst, a))
}
