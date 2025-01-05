# Pseudo code for completing the interpreter

## Folder structure:
- src/
  - parser/
    - parser.zig (main parser logic)
  - semantic/
    - semantic.zig (semantic analysis)
  - interpreter/
    - interpreter.zig (execution logic)
  - ast/
    - ast.zig (AST node definitions)
  - main.zig (entry point)
  - root.zig (utility functions)

## Pseudo code for parser.zig:
- Define a Parser struct with methods to parse the tokens into an AST
- Implement methods for parsing expressions, statements, and declarations
- Handle grammar rules and construct the AST nodes accordingly
- Report syntax errors with line and column numbers

## Pseudo code for semantic.zig:
- Define a SemanticAnalyzer struct with methods to analyze the AST
- Implement type checking, variable declaration checks, and other semantic rules
- Report semantic errors with meaningful messages

## Pseudo code for interpreter.zig:
- Define an Interpreter struct with methods to execute the AST
- Implement methods for evaluating expressions, executing statements, and handling control flow
- Manage variable assignments, function calls, and runtime behaviors
- Report runtime errors with context information

## Pseudo code for ast.zig:
- Define AST node types for different language constructs (e.g., expressions, statements, declarations)
- Each node type should have fields to store relevant information (e.g., operator, operands, etc.)
- Implement methods for traversing and manipulating the AST

## Pseudo code for main.zig:
- Initialize the scanner, parser, semantic analyzer, and interpreter
- Read the input source code
- Tokenize the source code using the scanner
- Parse the tokens into an AST using the parser
- Perform semantic analysis on the AST
- Execute the AST using the interpreter
- Handle and report any errors encountered during the process

## Pseudo code for root.zig:
- Define utility functions for common tasks (e.g., error reporting, memory management)
- Provide helper functions for working with the AST, tokens, and other data structures

## Example integration in main.zig:
```zig
var scanner = Scanner.init(source, allocator);
defer scanner.deinit();
try scanner.scan();

var parser = Parser.init(scanner.tokens, allocator);
defer parser.deinit();
var ast = try parser.parse();

var semanticAnalyzer = SemanticAnalyzer.init(ast, allocator);
defer semanticAnalyzer.deinit();
try semanticAnalyzer.analyze();

var interpreter = Interpreter.init(ast, allocator);
defer interpreter.deinit();
try interpreter.interpret();
```
