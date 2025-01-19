package parser

import (
	"fmt"
	"io"
	"strings"
)

// tracerI defines the required functionality
// for trace generation within the parser.
type tracerI interface {
	begin(string)
	end(string)
}

const (
	traceIndent    = "\t"
	traceMarkerBeg = ">"
	traceMarkerEnd = "<"
)

// tracer writes generated traces to the configured io.Writer.
type tracer struct {
	indent int
	out    io.Writer
}

func newTracer(out io.Writer) *tracer {
	return &tracer{indent: 0, out: out}
}

func (t *tracer) begin(msg string) {
	t.indent += 1
	_, _ = fmt.Fprintf(
		t.out,
		"%s%s %s\n",
		strings.Repeat(traceIndent, t.indent),
		traceMarkerBeg,
		msg,
	)
}

func (t *tracer) end(msg string) {
	_, _ = fmt.Fprintf(
		t.out,
		"%s%s %s\n",
		strings.Repeat(traceIndent, t.indent),
		traceMarkerEnd,
		msg,
	)
	t.indent -= 1
}

// dummyTracer provides a no-op tracer essentially,
// ignoring tracing.
type dummyTracer struct{}

func (dummyTracer) begin(_ string) {}

func (dummyTracer) end(_ string) {}
