package symbols

var builtInIdentifiers = []string{
	"print",
	"println",
	"read",
	"len",
}

// AnalyzeOption allows extending the default
// behaviour of Analyze.
type AnalyzeOption func(pass *symbolPass)

// WithBuiltin allows adding built in identifiers which
// will prevent errors during Analyze.
func WithBuiltin(identifiers ...string) AnalyzeOption {
	return func(pass *symbolPass) {
		builtInIdentifiers = identifiers
	}
}
