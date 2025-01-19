package riscv

import (
	"fmt"
	"slices"
)

type RegisterManager struct {
	tempPool []bool
}

func newRegisterManager() *RegisterManager {
	tempPool := make([]bool, 32)

	for _, reg := range []Register{t0, t1, t2, t3, t4, t5, t6} {
		tempPool[reg] = true
	}

	return &RegisterManager{tempPool: tempPool}
}

func (m *RegisterManager) allocTemp() Register {
	for i := range m.tempPool {
		if m.tempPool[i] {
			m.tempPool[i] = false
			return Register(i)
		}
	}
	panic("no free registers left!")
}

func (m *RegisterManager) free(reg Register) {
	// x0 == nil (e.g. zero register is always free)
	if reg == x0 {
		return
	}
	// a0...a7 are always available
	if slices.Contains([]Register{a0, a1, a2, a3, a4, a5, a6, a7}, reg) {
		return
	}

	if m.tempPool[reg] {
		panic(fmt.Errorf("bad free call: register '%v' is not in use", reg))
	}
	m.tempPool[reg] = true
}
