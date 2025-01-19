package riscv

type Register int

// TODO: improvement: there is definitely a better way to represent the registers
//
//	this was a quick hack to get the mnemonics instead of numeric
//	register names in the compilation output
var regToString = [32]string{
	"x0",
	"ra",
	"sp",
	"gp",
	"tp",
	"t0",
	"t1",
	"t2",
	"s0",
	"s1",
	"a0",
	"a1",
	"a2",
	"a3",
	"a4",
	"a5",
	"a6",
	"a7",
	"s2",
	"s3",
	"s4",
	"s5",
	"s6",
	"s7",
	"s8",
	"s9",
	"s10",
	"s11",
	"t3",
	"t4",
	"t5",
	"t6",
}

func (r Register) String() string {
	return regToString[r]
}

const (
	x0 = Register(iota)
	x1
	x2
	x3
	x4
	x5
	x6
	x7
	x8
	x9
	x10
	x11
	x12
	x13
	x14
	x15
	x16
	x17
	x18
	x19
	x20
	x21
	x22
	x23
	x24
	x25
	x26
	x27
	x28
	x29
	x30
	x31
)

// Register have alias names to make them easier to use.
const (
	// The zero Register always contains 0.
	zero = x0
	// ra = return address
	ra = x1
	// sp = stack pointer
	sp = x2
	// gp = global pointer
	gp = x3
	// tp = thread pointer
	tp = x4
	// t0 = temporary 0
	t0 = x5
	// t1 = temporary 1
	t1 = x6
	// t2 = temporary 2
	t2 = x7
	// s0 or fp = saved register 0 or frame pointer
	s0 = x8
	// s1 = saved register 1
	s1 = x9
	// a0 through a7 are function arguments
	// a0 and a1 are used as return values
	a0  = x10
	a1  = x11
	a2  = x12
	a3  = x13
	a4  = x14
	a5  = x15
	a6  = x16
	a7  = x17
	s2  = x18
	s3  = x19
	s4  = x20
	s5  = x21
	s6  = x22
	s7  = x23
	s8  = x24
	s9  = x25
	s10 = x26
	s11 = x27
	t3  = x28
	t4  = x29
	t5  = x30
	t6  = x31
)

var argRegisters = []Register{
	a0, a1, a2, a3, a4, a5, a6, a7,
}
