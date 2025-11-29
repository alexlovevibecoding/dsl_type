# DSL Type Parser

F# implementation of the type-language described in `grammar.md`. The repo contains:

- `src/DslType.Parser` – FsLex/FsYacc-based lexer, parser, and AST.
- `src/DslType.Cli` – console entry point that parses input (stdin or file) and prints it via the `Pretty` module.
- `tests/DslType.Tests` – xUnit tests that exercise core grammar constructs.
- `examples.md` – sample inputs the parser already understands.

## Getting Started

```bash
dotnet build dsl_type.sln
```

### Run the CLI

```bash
dotnet run --project src/DslType.Cli/DslType.Cli.fsproj -- examples.md
```

Omit the file argument to read from standard input.

### Run the Tests

```bash
dotnet test dsl_type.sln
```

## Project Layout

```
src/
  DslType.Parser/   # AST + lexer (.fsl) + parser (.fsy)
  DslType.Cli/      # console runner referencing the parser library
tests/
  DslType.Tests/    # parser-focused xUnit test suite
```

FsLex/FsYacc artifacts are generated at build time; `bin/` and `obj/` are ignored via `.gitignore`.
