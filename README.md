# z-interpreter

A Zig-based interpreter implementation featuring a lexical scanner. This project demonstrates how to tokenize source code into meaningful tokens that can be used by a compiler or interpreter.

## Features

- Lexical scanner implementation in Zig
- Tokenizes source code into:
  - Identifiers
  - Numbers
  - Strings
  - Operators
  - Keywords
  - Punctuation
- Handles:
  - Single and multi-line comments
  - Whitespace
  - Error tokens
  - Line tracking

## Getting Started

### Prerequisites

- Zig compiler (0.13)
- Basic understanding of compiler/interpreter concepts

### Running the Scanner

1. Clone the repository
2. Navigate to the project directory
3. Run the scanner:

```bash
zig build run
```

This will:
- Tokenize the sample code in `src/main.zig`
- Print all tokens to stdout

### Running Tests

To run the scanner tests:

```bash
zig build test
```

This will:
- Run the test suite in `src/scanner/scanner.zig`
- Verify tokenization of sample code
- Check for correct token types and positions

## Sample Output

Running the scanner will produce output like:

```
Tokens:
[Line 1] Var: var
[Line 1] Identifier: x
[Line 1] Equal: =
[Line 1] Number: 42
[Line 1] Semicolon: ;
[Line 2] If: if
[Line 2] LeftParen: (
[Line 2] Identifier: x
[Line 2] Greater: >
[Line 2] Number: 10
[Line 2] RightParen: )
[Line 2] LeftBrace: {
[Line 3] Print: print
[Line 3] LeftParen: (
[Line 3] String: "Hello, World!"
[Line 3] RightParen: )
[Line 3] Semicolon: ;
[Line 4] RightBrace: }
[Line 4] Eof:
```

## Project Structure

- `src/main.zig`: Main entry point with sample code
- `src/scanner/`: Scanner implementation
  - `scanner.zig`: Core scanner logic
