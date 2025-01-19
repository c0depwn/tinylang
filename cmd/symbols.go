package cmd

import (
	"fmt"
	"github.com/c0depwn/tinylang/ast"
	"github.com/c0depwn/tinylang/parser"
	"github.com/c0depwn/tinylang/symbols"
	"github.com/spf13/cobra"
	"strings"
)

func newSymbolsCommand() *cobra.Command {
	return &cobra.Command{
		Use:   "symbols [source_file]",
		Short: "Show the output of the symbol analysis",
		Args:  cobra.ExactArgs(1),
		RunE:  runSymbols,
	}
}

func runSymbols(_ *cobra.Command, args []string) error {
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

	info, err := symbols.Analyze(astFile)
	if err != nil {
		return err
	}

	fmt.Printf("+ %[1]s + %[1]s + %[1]s +\n", strings.Repeat("-", 16))
	fmt.Printf("| %16s | %16s | %16s |\n", "declaration name", "belongs to", "defines")
	fmt.Printf("+ %[1]s + %[1]s + %[1]s +\n", strings.Repeat("-", 16))

	symbols.Inspect(
		astFile, info,
		func(node ast.Node, scope *symbols.Scope) {
			if block, ok := node.(*ast.Block); ok {
				defines := "ø"
				if definedBy := info.DefinedBy(block); definedBy != nil {
					defines = fmt.Sprintf("%p", definedBy)
				}

				fmt.Printf("| %16s | %16p | %16s |\n", "block", scope, defines)
			}
			if decl, ok := node.(ast.Declaration); ok {
				defines := "ø"
				if definedBy := info.DefinedBy(node); definedBy != nil {
					defines = fmt.Sprintf("%p", definedBy)
				}

				fmt.Printf("| %16s | %16p | %16s |\n", decl.Name(), scope, defines)
			}
		},
		func(node ast.Node, scope *symbols.Scope) {},
	)

	fmt.Printf("+ %[1]s + %[1]s + %[1]s +\n", strings.Repeat("-", 16))

	return nil
}
