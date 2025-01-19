package symbols

import (
	"testing"
)

func TestPass(t *testing.T) {
	f := newFunc("f")
	p := newSymbolPass()

	p.startScope(f)
	if p.current == nil {
		t.Fatal("bad state in symbolPass after startScope")
	}

	p.endScope()
	if p.current != nil {
		t.Fatal("bad state in symbolPass after endScope")
	}
}

func TestPass_Assertions(t *testing.T) {
	mustPanic(t, func() {
		p := newSymbolPass()
		p.startScope(nil)
	})

	mustPanic(t, func() {
		p := newSymbolPass()
		p.endScope()
	})
}

func mustPanic(t *testing.T, f func()) {
	defer func() {
		if p := recover(); p != nil {
			if e, ok := p.(error); ok {
				t.Logf("panic (is error): %v", e)
			} else {
				t.Logf("panic (isn't error): %v", p)
			}
			return // ok
		}
		t.Fatalf("did not panic")
	}()

	f()
}
