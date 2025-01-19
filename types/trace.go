package types

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"io"
	"strings"
)

type trace interface {
	pre(ast.Node)
	post(ast.Node)
	info(string)
}

type tracer struct {
	indent int
	out    io.Writer
}

func newTracer(out io.Writer) *tracer {
	return &tracer{out: out}
}

func (t *tracer) pre(node ast.Node) {
	t.indent += 1
	_, _ = fmt.Fprintf(t.out, "%s> %T\n", strings.Repeat(" ", t.indent), node)
}

func (t *tracer) post(node ast.Node) {
	_, _ = fmt.Fprintf(t.out, "%s< %T\n", strings.Repeat(" ", t.indent), node)
	t.indent -= 1
}

func (t *tracer) info(msg string) {
	_, _ = fmt.Fprintf(t.out, "%s\n", msg)
}

type dummyTracer struct{}

func (d dummyTracer) pre(node ast.Node) {}

func (d dummyTracer) post(node ast.Node) {}

func (d dummyTracer) info(_ string) {}
