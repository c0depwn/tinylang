package riscv

import "fmt"

// environment is used to save and retrieve the offset
// (from the frame pointer) of stack-allocated variables
// and arguments.
// Every function declaration uses a new environment.
// Therefore, all local variables can only be found
// (using lookup) within the same environment.
type environment struct {
	labelCounter int
	offset       int
	locals       map[string]int
}

// register a variable of a specific size for the current environment.
func (e *environment) register(name string, size int) {
	e.offset -= size
	e.locals[name] = e.offset
}

// lookup returns the offset (relative to the stack frame) for the supplied variable.
func (e *environment) lookup(name string) int {
	offset, ok := e.locals[name]
	if !ok {
		panic(fmt.Errorf("missing local %s from environment", name))
	}
	return offset
}

func (e *environment) provideLabel() string {
	l := fmt.Sprintf("L_%d", e.labelCounter)
	e.labelCounter++
	return l
}
