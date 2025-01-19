package parser

import (
	"errors"
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/lexer"
	"github.com/c0depwn/tinylang/pkg/ext"
	"io"
)

// The ErrorHandler decides what to do when an error is encountered during parsing.
type ErrorHandler func(err error)

// PanicErrHandler simply panics on the first error which is encountered during parsing.
func PanicErrHandler(err error) { panic(err) }

// LexerTokenSource provides a TokenSource which is implemented by the [lexer] package.
// Comments will automatically be ignored when using LexerTokenSource.
func LexerTokenSource(r io.Reader) TokenSource {
	return ignoreCommentsFrom(lexer.New(r))
}

// Option defines the type for parser customization options.
type Option func(*parser)

// EnableDebug enables additional assertions during parsing.
// Caution is advised, the parser may terminate prematurely on incorrect input.
func EnableDebug() Option {
	return func(p *parser) { p.debug = true }
}

// EnableTrace writes a trace of the called parser functions to the provided io.Writer.
// This can be useful to investigate the parser.
func EnableTrace(out io.Writer) Option {
	return func(p *parser) { p.tracer = newTracer(out) }
}

// ParseFile using the provided TokenSource and ErrorHandler.
// The result is an [*ast.File] representing the AST.
func ParseFile(src TokenSource, errHandler ErrorHandler, opts ...Option) (f *ast.File, err error) {
	if src == nil {
		return nil, errors.New("token source cannot be nil")
	}
	if errHandler == nil {
		return nil, errors.New("error handler cannot be nil")
	}

	p := newParser(src, errHandler)

	for _, option := range opts {
		option(p)
	}

	err = ext.CatchPanic(func() {
		f = p.parse()
	})

	return f, err
}

func newParser(src TokenSource, errH ErrorHandler) *parser {
	p := new(parser)
	p.tokens = src
	p.errHandler = errH
	p.tracer = dummyTracer{}

	// register parse functions for infix and prefix expressions
	p.parseFuncs = newParseFuncs(p)

	// pre-fill current and lookahead token
	p.advance()
	p.advance()

	return p
}
