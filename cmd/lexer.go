package cmd

import (
	"fmt"
	"github.com/c0depwn/tinylang/lexer"
	"github.com/c0depwn/tinylang/token"
	"github.com/spf13/cobra"
)

var lexCmd = &cobra.Command{
	Use:   "lex [source_file]",
	Short: "Show the output of the lexical analysis",
	Args:  cobra.ExactArgs(1),
	RunE:  runLexer,
}

func runLexer(cmd *cobra.Command, args []string) error {
	f, err := openFile(args[0])
	if err != nil {
		return err
	}

	l := lexer.New(f)

	for {
		t := l.Next()

		if t.Type == token.EOF {
			break
		}
		if t.Type == token.Illegal {
			return fmt.Errorf("illegal input: %s", t.Literal)
		}

		fmt.Println(t)
	}

	return nil
}
