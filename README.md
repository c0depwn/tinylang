# tinylang

TinyLang, a lightweight, educational programming language designed to introduce students to the principles of compiler
construction. The compiler currently only targets a small subset of RISC-V, specifically RV32IM and RV64IM.

Warning: TinyLang comes with no guarantees and is still considered highly experimental at this point. Use at your own
risk. There are `// TODO` comments marking improvements or open issue throughout the source code.

## Grammar

The grammar definition can be found [here](docs/resources/grammar.txt).

## Language Examples

Some language examples can be found within the [examples directory](docs/examples).

## CLI

The CLI provides various command which expose information related to some stages of the compiler.
Additionally, debugging and tracing functionality is partially available, depending on the command.
The following assumes that you are on a macOS or linux system and have installed [go](https://go.dev/).
Viewing all available commands can be achieved by using the `--help` flag:

```shell
go run ./ --help
```

### Lexer

The lexer output of a given source file can be printed to stdout using the `lex` command.

```shell
# prints out every token and its position
go run ./ lex <source-file>
```

### Parser

The parse command gives some insight into the parsing process. Additional flags can be specified which provide output
useful for understanding how the parser behaves.

```shell
# enables additional assertions during the parsing process
go run ./ parse --debug <source-file>
# prints a trace of the functions which were traversed during parsing,
# this output resembles the produced AST
go run ./ parse --trace <source-file>
# flags can be combined
go run ./ parse --debug --trace <source-file>
```

### Symbols

The symbols command prints collected symbol information. The printed table shows each declaration, to what scope it
belongs and which scope it defines, if it defines a new scope. The scope is represented using a number which represents
the pointer of the scope object holding the scope information.

```shell
go run ./ symbols <source-file>
```

### Type Information

The `types` command provides type information for declarations. The output corresponds to the collected type
information. Similar to `parse`, `types` provides a `--trace` flag which allows tracing the traversal of the type
checker through the AST.

```shell
go run ./ types <source-file>
```

### Compile

Compiling a source code file can be done using the `compile` command. It accepts a `target` and `arch` flag. Invoke the
command with `--help` for details.

```shell
# will output compiled assembly to stdout
go run ./ compile <source-file>
# to save the result to a file use >
go run ./ compile <source-file> > program.s
```

#### Running in Ripes

Compiled programs can be executed within the [Ripes](https://github.com/mortbopet/Ripes) simulator. It must be ensured
that the specified flags during compilation are correct, `--target=ripes` and the `--arch` must match the selected CPU
architecture within ripes (32/64-bit).

#### Real RISC-V Platform, Assembling and Linking with GNU libc

Emulating the RISC-V platform is possible through docker. Notice, this is true for at least macOS, other platforms were
not tested. This assumes that you have installed [docker](https://www.docker.com/). The included `docker-compose.yaml`
can be used to run an ubuntu container.

```shell
# start container
docker compose up -d
# compile your program and save the output in the workspace directory
# alternatively, you can also directly create and edit your program within the workspace folder
go run ./ compile --target=libc --arch=rv64 myprogram.tl > ./workspace/myprogram.s
# open shell in container
docker exec -ti ubuntu bash 
# navigate to /app, this mirrors the contents of ./workspace on your host
cd /app
# assemble and link with libc using the risc-v tool chain
riscv64-linux-gnu-gcc -o myprogram myprogram.s -static
# run the program
./myprogram
```

Warning: This is highly experimental and functionality is very limited at the moment. Partially due to violations to
the ABI and other things like strings not being null terminated like in C. A good example which actually works when
removing all print calls involving strings is [bubblesort.tl](./docs/examples/bubblesort.tl).

## Building

To build the compiler directly you can run `go build -o tinylang` in the root of the repository.
Alternatively, if the `$GOPATH` is properly setup, running `go install` will automatically build and place the binary
into `$GOBIN`, which should make it available in your `$PATH`, enabling you to directly run `tinylang` after the
`install` command.

## Testing

```shell
# test all packages
go test ./...

# test specific package
go test ./<package>/...

# test and output coverage
go test -cover ./<package>/...

# test with coverage profile and display coverage
go test -coverprofile=coverage.out ./<package>/...

# test with coverage profile and display coverage in browser
go test -coverprofile=coverage.out ./lexer/...
go test -coverprofile=coverage.out ./parser/...
go tool cover -html=coverage.out
```

