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
