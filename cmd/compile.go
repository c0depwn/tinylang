package cmd

import (
	"fmt"
	"github.com/c0depwn/tinylang/codegen/riscv"
	"github.com/c0depwn/tinylang/parser"
	"github.com/spf13/cobra"
	"os"
)

var (
	compileFlagArch   string
	compileFlagTarget string
)

func newCompileCommand() *cobra.Command {
	compileCmd := &cobra.Command{
		Use:   "compile [source_file]",
		Short: "Compile a tinylang source file",
		Args:  cobra.ExactArgs(1),
		RunE:  runCompiler,
	}
	compileCmd.PersistentFlags().StringVar(&compileFlagArch, "arch", "rv32", "architecture, one of [rv32-ripes, rv32, rv64]")
	compileCmd.PersistentFlags().StringVarP(&compileFlagTarget, "target", "t", "ripes", "target, on of [linux, ripes]")
	return compileCmd
}

func runCompiler(cmd *cobra.Command, args []string) error {
	f, err := os.Open(args[0])
	if err != nil {
		return err
	}
	stat, err := f.Stat()
	if err != nil {
		return err
	}
	if stat.IsDir() {
		return fmt.Errorf("'%s' is a directory, please provide a file", args[0])
	}

	astFile, err := parser.ParseFile(
		parser.LexerTokenSource(f),
		parser.PanicErrHandler,
	)
	if err != nil {
		return err
	}

	return riscv.Generate(
		astFile, os.Stdout,
		riscv.WithTarget(riscv.Target(compileFlagTarget)),
		riscv.WithPlatform(riscv.PlatformID(compileFlagArch)),
	)
}
