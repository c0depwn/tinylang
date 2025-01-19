package initialization

import (
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/symbols"
)

// Analyze the initialization order of global variables.
// If no error is returned each global ast.Declaration will have their order set.
// Sorting the declarations is left to the caller.
func Analyze(f *ast.File, info symbols.Info) error {
	info.Before(f)
	defer info.After(f)

	if err := order(f.Declarations, info); err != nil {
		return err
	}
	return nil
}
