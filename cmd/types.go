package cmd

import (
	"fmt"
	"github.com/c0depwn/tinylang/parser"
	"github.com/c0depwn/tinylang/symbols"
	"github.com/c0depwn/tinylang/types"
	"github.com/spf13/cobra"
	"os"
	"strings"
)

var (
	typesFlagTrace bool
)

func newTypesCommand() *cobra.Command {
	typesCmd := &cobra.Command{
		Use:   "types [source_file]",
		Short: "Output of type analysis",
		Args:  cobra.ExactArgs(1),
		RunE:  runTypes,
	}

	typesCmd.PersistentFlags().BoolVar(&typesFlagTrace, "trace", false, "enable trace information output")

	return typesCmd
}

func runTypes(cmd *cobra.Command, args []string) error {
	f, err := openFile(args[0])
	if err != nil {
		return err
	}

	astFile, err := parser.ParseFile(
		parser.LexerTokenSource(f),
		parser.PanicErrHandler,
	)
	if err != nil {
		return err
	}

	opts := []types.AnalyzeOption{}
	if typesFlagTrace {
		opts = append(opts, types.WithTraversalTrace(os.Stdout))
	}

	info, err := symbols.Analyze(astFile)
	if err != nil {
		return err
	}

	if err := types.Analyze(astFile, info, opts...); err != nil {
		return err
	}

	fmt.Printf("+ %[1]s + %[1]s +\n", strings.Repeat("-", 16))
	fmt.Printf("| %16s | %16s | \n", "name", "type")
	fmt.Printf("+ %[1]s + %[1]s +\n", strings.Repeat("-", 16))

	fileScope := info.DefinedBy(astFile)
	for _, decl := range fileScope.AllDeclarations() {
		fmt.Printf("| %16s | %16s |\n", decl.Name(), decl.Type())
	}

	fmt.Printf("+ %[1]s + %[1]s +\n", strings.Repeat("-", 16))

	return nil
}
