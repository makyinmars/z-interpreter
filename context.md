# Interpreter Stages

## Scanning
- Empty file
- Parentheses
- Braces
- Other single-character tokens
- Lexical errors
- Assignment & equality Operators
- Negation & inequality operators
- Relational operators
- Division operator & comments
- Whitespace
- Multi-line errors
- String literals
- Number literals
- Identifiers
- Reserved words

**Steps for Scanner Implementation**

1. Create a folder structure:
   - src/
     - scanner/
       - scanner.zig (main scanner logic)
     - main.zig (entry point)
     - root.zig (utility functions)

2. Implement the scanner logic in scanner.zig:
   - Define a TokenType enum for all token types (e.g., Identifier, Number, String, etc.)
   - Create a Token struct to hold token type and value
   - Implement a Scanner struct with methods to scan the input:
     - scan(): Main method to scan the entire input
     - scanToken(): Scan a single token
     - scanIdentifier(): Scan identifiers and reserved words
     - scanNumber(): Scan number literals
     - scanString(): Scan string literals
     - skipWhitespace(): Skip whitespace characters
     - handleComments(): Handle single-line and multi-line comments
     - handleErrors(): Handle lexical errors

3. Pseudo code for scanning process:
   - Initialize the scanner with the input source
   - While not at the end of the input:
     - Skip whitespace and comments
     - Identify the next token type based on the current character
     - Call the appropriate scan method (e.g., scanIdentifier, scanNumber, etc.)
     - Create a Token and add it to the token list
     - Handle any lexical errors encountered
   - Return the list of tokens

4. Implement error handling:
   - Report errors with line and column numbers
   - Handle multi-line errors gracefully

5. Test the scanner:
   - Create test cases for each token type
   - Verify that the scanner correctly identifies tokens and handles errors

6. Integrate the scanner with the main interpreter:
   - Modify main.zig to use the scanner
   - Print the list of tokens for debugging purposes

7. Build and run the interpreter:
   - Use the Zig build system to compile the project
   - Run the interpreter with sample input to verify the scanner works correctly

## Parsing Expressions
- Booleans & Nil
- Number literals
- String literals
- Parentheses
- Unary Operators
- Arithmetic operators (1/2)
- Arithmetic operators (2/2)
- Comparison operators
- Equality operators
- Syntactic errors

## Evaluating Expressions
- Literals: Booleans & Nil
- Literals: Strings & Numbers
- Parentheses
- Unary Operators: Negation & Not
- Arithmetic Operators (1/2)
- Arithmetic Operators (2/2)
- String Concatenation
- Relational Operators
- Equality Operators
- Runtime Errors: Unary Operators
- Runtime Errors: Binary Operators (1/2)
- Runtime Errors: Binary Operators (2/2)
- Runtime Errors: Relational Operators

## Statements & State
- Print: Generate output
- Print: Multiple statements
- Expression statements
- Variables: Declare variables
- Variables: Runtime Errors
- Variables: Initialize variables
- Variables: Redeclare variables
- Assignment operation
- Block syntax
- Scopes

## Control Flow
- If statements
- Else statements
- Else-if statements
- Nested if statements
- Logical OR operator
- Logical AND operator
- While statements
- For statements
- Syntactic errors

## Functions
- Native functions
- Functions without arguments
- Functions with arguments
- Syntax errors
- Return statements
- Higher order functions
- Runtime errors
- Function scope
- Closures

To complete the interpreter and transition to creating a compiler, you need to follow these steps:

### Steps to Complete the Interpreter:

1. **Abstract Syntax Tree (AST) Construction**:
   - After tokenizing the source code, you need to parse the tokens into an Abstract Syntax Tree (AST). The AST represents the syntactic structure of the program.
   - Define AST node types for different language constructs (e.g., expressions, statements, declarations).

2. **Parser Implementation**:
   - Implement a parser that takes the tokens produced by the scanner and constructs the AST.
   - The parser should handle grammar rules for expressions, statements, and declarations.

3. **Semantic Analysis**:
   - Implement semantic analysis to check for type correctness, variable declarations, and other semantic rules.
   - This step ensures that the program is semantically valid before execution.

4. **Interpreter Execution**:
   - Implement an interpreter that walks the AST and executes the program.
   - The interpreter should handle variable assignments, control flow, function calls, and other runtime behaviors.

5. **Error Handling**:
   - Enhance error handling to provide meaningful error messages during parsing, semantic analysis, and execution.

### Steps to Create a Compiler:

1. **Intermediate Representation (IR)**:
   - Define an intermediate representation (IR) for your compiler. This could be a low-level representation like LLVM IR or a custom IR.
   - The IR should be easier to optimize and translate to machine code.

2. **Code Generation**:
   - Implement a code generator that traverses the AST and produces the IR.
   - The code generator should handle different language constructs and map them to the appropriate IR instructions.

3. **Optimization**:
   - Implement optimization passes on the IR to improve the performance of the generated code.
   - Common optimizations include constant folding, dead code elimination, and loop unrolling.

4. **Target Code Generation**:
   - Implement a backend that translates the optimized IR into machine code for the target architecture.
   - This step involves generating assembly code or directly producing binary executables.

5. **Linking and Assembly**:
   - If your compiler generates object files, implement a linker to combine them into a single executable.
   - Alternatively, if generating assembly, use an assembler to produce the final binary.

6. **Integration with Interpreter**:
   - You can integrate the compiler with the interpreter by allowing the interpreter to optionally compile the source code to an executable.
   - Alternatively, you can use the interpreter as a runtime for the compiled code, especially if the compiled code relies on runtime features like garbage collection.

### Example Integration:

```zig
// Example of integrating a compiler with the interpreter
pub const Compiler = struct {
    ast: *AST, // Assume AST is defined elsewhere
    ir: *IR,   // Assume IR is defined elsewhere

    pub fn compile(self: *Compiler, source: []const u8) !void {
        var scanner = Scanner.init(source, self.allocator);
        defer scanner.deinit();
        try scanner.scan();

        var parser = Parser.init(scanner.tokens, self.allocator);
        defer parser.deinit();
        self.ast = try parser.parse();

        var codegen = CodeGenerator.init(self.ast, self.allocator);
        defer codegen.deinit();
        self.ir = try codegen.generateIR();

        var optimizer = Optimizer.init(self.ir, self.allocator);
        defer optimizer.deinit();
        try optimizer.optimize();

        var backend = Backend.init(self.ir, self.allocator);
        defer backend.deinit();
        try backend.generateMachineCode();
    }
};

// Example usage
test "Compiler Integration" {
    const allocator = testing.allocator;
    const source = "var x = 42; print(x);";

    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    try compiler.compile(source);

    // Optionally, you can run the compiled code using the interpreter
    var interpreter = Interpreter.init(compiler.ast, allocator);
    defer interpreter.deinit();
    try interpreter.interpret();
}
```

### Summary:

1. **Complete the Interpreter**: Implement parsing, semantic analysis, and execution.
2. **Create the Compiler**: Define an IR, implement code generation, optimization, and target code generation.
3. **Integrate**: Allow the interpreter to optionally compile code or use the interpreter as a runtime for compiled code.

By following these steps, you can transition from an interpreter to a full-fledged compiler while maintaining the ability to interpret code.
