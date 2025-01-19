package cmd

import (
	"github.com/c0depwn/tinylang/parser"
	"github.com/spf13/cobra"
	"os"
)

var (
	parseFlagDebug bool
	parseFlagTrace bool
)

func newParseCommand() *cobra.Command {
	parseCmd := &cobra.Command{
		Use:   "parse [source_file]",
		Short: "Invoke the parser",
		Args:  cobra.ExactArgs(1),
		RunE:  runParser,
	}

	parseCmd.PersistentFlags().BoolVar(&parseFlagDebug, "debug", false, "enable additional debugging asserts")
	parseCmd.PersistentFlags().BoolVar(&parseFlagTrace, "trace", false, "enable trace of called parse functions")

	return parseCmd
}

func runParser(cmd *cobra.Command, args []string) error {
	f, err := openFile(args[0])
	if err != nil {
		return err
	}

	var parserOptions []parser.Option
	if parseFlagDebug {
		parserOptions = append(parserOptions, parser.EnableDebug())
	}
	if parseFlagTrace {
		parserOptions = append(parserOptions, parser.EnableTrace(os.Stdout))
	}

	_, err = parser.ParseFile(
		parser.LexerTokenSource(f),
		parser.PanicErrHandler,
		parserOptions...,
	)
	if err != nil {
		return err
	}

	return err
}
