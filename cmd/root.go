package cmd

import (
	"fmt"
	"github.com/spf13/cobra"
	"os"
)

var rootCmd = &cobra.Command{
	Use:   "tinylang",
	Short: "The TinyLang programming language",
}

func Exec() {
	rootCmd.AddCommand(lexCmd)
	rootCmd.AddCommand(newParseCommand())
	rootCmd.AddCommand(newSymbolsCommand())
	rootCmd.AddCommand(newTypesCommand())
	rootCmd.AddCommand(newCompileCommand())

	rootCmd.SilenceUsage = true
	rootCmd.SilenceErrors = true

	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func openFile(path string) (*os.File, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	stat, err := f.Stat()
	if err != nil {
		return nil, err
	}
	if stat.IsDir() {
		return nil, fmt.Errorf("'%s' is a directory, please provide a file", path)
	}
	return f, nil
}
