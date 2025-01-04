const std = @import("std");
const Scanner = @import("../src/scanner/scanner.zig").Scanner;
const TokenType = @import("../src/scanner/scanner.zig").TokenType;

const testing = std.testing;

test "Scanner with source" {

    // Initialize the allocator (e.g., using the testing allocator)
    const allocator = testing.allocator;

    // Define a simple source code string to scan
    const source =
        \\var x = 42;
        \\if (x > 10) {
        \\    print("Hello, World!");
        \\}
    ;

    // Initialize the scanner with the source code
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit(); // Ensure the scanner is deinitialized after the test

    // Scan the source code and collect tokens
    try scanner.scan();

    // Define the expected tokens
    const expected_tokens = [_]TokenType{
        TokenType.Var,
        TokenType.Identifier,
        TokenType.Equal,
        TokenType.Number,
        TokenType.Semicolon,
        TokenType.If,
        TokenType.LeftParen,
        TokenType.Identifier,
        TokenType.Greater,
        TokenType.Number,
        TokenType.RightParen,
        TokenType.LeftBrace,
        TokenType.Print,
        TokenType.LeftParen,
        TokenType.String,
        TokenType.RightParen,
        TokenType.Semicolon,
        TokenType.RightBrace,
        TokenType.Eof,
    };

    // Verify that the number of tokens matches the expected number
    try testing.expectEqual(expected_tokens.len, scanner.tokens.items.len);

    // Verify each token's type matches the expected type
    for (scanner.tokens.items, expected_tokens) |token, expected_type| {
        try testing.expectEqual(expected_type, token.type);
    }
}

// Test the scanner with an empty input string.
// Expected Output: Only an `EOF` token should be generated.
test "Empty Input" {
    const allocator = testing.allocator;
    const source = "";
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();
    try scanner.scan();
    try testing.expectEqual(1, scanner.tokens.items.len);
    try testing.expectEqual(TokenType.Eof, scanner.tokens.items[0].type);
}

// - Test the scanner with a string containing only whitespace characters (spaces, tabs, newlines).
// - Expected Output: Only an `EOF` token should be generated, and the line counter should be incremented for each newline.
test "Whitespace Only" {
    const allocator = testing.allocator;
    const source = " \t\n\r ";
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();
    try scanner.scan();
    try testing.expectEqual(1, scanner.tokens.items.len);
    try testing.expectEqual(TokenType.Eof, scanner.tokens.items[0].type);
    try testing.expectEqual(2, scanner.tokens.items[0].line); // Line counter should be 2 due to newline
}

// - Test the scanner with each single-character token (e.g., `(`, `)`, `{`, `}`, `,`, `.`, `-`, `+`, `;`, `/`, `*`).
// - Expected Output: Each character should be correctly tokenized.
test "Single Character Tokens" {
    const allocator = testing.allocator;
    const source = "(){}/.,-+;*";
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();
    try scanner.scan();
    const expected_tokens = [_]TokenType{
        TokenType.LeftParen,
        TokenType.RightParen,
        TokenType.LeftBrace,
        TokenType.RightBrace,
        TokenType.Slash,
        TokenType.Dot,
        TokenType.Comma,
        TokenType.Minus,
        TokenType.Plus,
        TokenType.Semicolon,
        TokenType.Star,
        TokenType.Eof,
    };
    try testing.expectEqual(expected_tokens.len, scanner.tokens.items.len);
    for (scanner.tokens.items, expected_tokens) |token, expected_type| {
        try testing.expectEqual(expected_type, token.type);
    }
}

// - Test the scanner with multi-character tokens (e.g., `!=`, `==`, `<=`, `>=`).
// - Expected Output: Each multi-character token should be correctly identified.
test "Multi-Character Tokens" {
    const allocator = testing.allocator;
    const source = "!= == <= >=";
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();
    try scanner.scan();
    const expected_tokens = [_]TokenType{
        TokenType.BangEqual,
        TokenType.EqualEqual,
        TokenType.LessEqual,
        TokenType.GreaterEqual,
        TokenType.Eof,
    };
    try testing.expectEqual(expected_tokens.len, scanner.tokens.items.len);
    for (scanner.tokens.items, expected_tokens) |token, expected_type| {
        try testing.expectEqual(expected_type, token.type);
    }
}

// - Test the scanner with various string literals, including multi-line strings and unterminated strings.
// - Expected Output: String literals should be correctly tokenized,
//   and unterminated strings should generate an error token.
test "String Literals" {
    const allocator = testing.allocator;
    const source = "\"Hello, World!\" \"Multi\nLine\" \"Unterminated";
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();
    try scanner.scan();
    const expected_tokens = [_]TokenType{
        TokenType.String,
        TokenType.String,
        TokenType.Error,
        TokenType.Eof,
    };
    try testing.expectEqual(expected_tokens.len, scanner.tokens.items.len);
    for (scanner.tokens.items, expected_tokens) |token, expected_type| {
        try testing.expectEqual(expected_type, token.type);
    }
}

// - Test the scanner with various number literals, including integers and floating-point numbers.
// - Expected Output: Number literals should be correctly tokenized.
test "Number Literals" {
    const allocator = testing.allocator;
    const source = "42 3.14 0.123 123.456";
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();
    try scanner.scan();
    const expected_tokens = [_]TokenType{
        TokenType.Number,
        TokenType.Number,
        TokenType.Number,
        TokenType.Number,
        TokenType.Eof,
    };
    try testing.expectEqual(expected_tokens.len, scanner.tokens.items.len);
    for (scanner.tokens.items, expected_tokens) |token, expected_type| {
        try testing.expectEqual(expected_type, token.type);
    }
}

// - Test the scanner with various identifiers and reserved keywords.
// - Expected Output: Identifiers should be correctly tokenized, and reserved keywords should be recognized as their respective token types.
test "Identifiers and Keywords" {
    const allocator = testing.allocator;
    const source = "var x = if else while true false";
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();
    try scanner.scan();
    const expected_tokens = [_]TokenType{
        TokenType.Var,
        TokenType.Identifier,
        TokenType.Equal,
        TokenType.If,
        TokenType.Else,
        TokenType.While,
        TokenType.True,
        TokenType.False,
        TokenType.Eof,
    };
    try testing.expectEqual(expected_tokens.len, scanner.tokens.items.len);
    for (scanner.tokens.items, expected_tokens) |token, expected_type| {
        try testing.expectEqual(expected_type, token.type);
    }
}

// - Test the scanner with single-line and multi-line comments.
// - Expected Output: Comments should be ignored, and no tokens should be generated for them.
test "Comments" {
    const allocator = testing.allocator;
    const source = "// Single-line comment\n/* Multi-line\ncomment */ var x = 42;";
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();
    try scanner.scan();
    const expected_tokens = [_]TokenType{
        TokenType.Var,
        TokenType.Identifier,
        TokenType.Equal,
        TokenType.Number,
        TokenType.Semicolon,
        TokenType.Eof,
    };
    try testing.expectEqual(expected_tokens.len, scanner.tokens.items.len);
    for (scanner.tokens.items, expected_tokens) |token, expected_type| {
        try testing.expectEqual(expected_type, token.type);
    }
}

// - Test the scanner with invalid characters or unexpected input.
// - Expected Output: Error tokens should be generated for invalid input.
test "Error Handling" {
    const allocator = testing.allocator;
    const source = "@#$\nvar x = 42;";
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();
    try scanner.scan();
    const expected_tokens = [_]TokenType{
        TokenType.Error,
        TokenType.Error,
        TokenType.Error,
        TokenType.Var,
        TokenType.Identifier,
        TokenType.Equal,
        TokenType.Number,
        TokenType.Semicolon,
        TokenType.Eof,
    };
    try testing.expectEqual(expected_tokens.len, scanner.tokens.items.len);
    for (scanner.tokens.items, expected_tokens) |token, expected_type| {
        try testing.expectEqual(expected_type, token.type);
    }
}

// - Test the scanner with a mix of all the above cases to ensure it handles complex input correctly.
// - Expected Output: All tokens should be correctly identified and categorized.
test "Mixed Input" {
    const allocator = testing.allocator;
    const source = "var x = 42;\nif (x > 10) {\n    print(\"Hello, World!\");\n} // Comment\n/* Multi-line\ncomment */";
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();
    try scanner.scan();
    const expected_tokens = [_]TokenType{
        TokenType.Var,
        TokenType.Identifier,
        TokenType.Equal,
        TokenType.Number,
        TokenType.Semicolon,
        TokenType.If,
        TokenType.LeftParen,
        TokenType.Identifier,
        TokenType.Greater,
        TokenType.Number,
        TokenType.RightParen,
        TokenType.LeftBrace,
        TokenType.Print,
        TokenType.LeftParen,
        TokenType.String,
        TokenType.RightParen,
        TokenType.Semicolon,
        TokenType.RightBrace,
        TokenType.Eof,
    };
    try testing.expectEqual(expected_tokens.len, scanner.tokens.items.len);
    for (scanner.tokens.items, expected_tokens) |token, expected_type| {
        try testing.expectEqual(expected_type, token.type);
    }
}

// - Test the scanner by creating a function called `add` that takes two numbers and returns their sum.
// - Expected Output: The scanner should correctly tokenize the function definition, including the function name, parameters, and return statement.
// - This test ensures that the scanner can handle function definitions, including identifiers, numbers, and operators.
test "Function Definition" {
    const allocator = testing.allocator;
    const source =
        \\fun adding(a, b) {
        \\    return a + b;
        \\}
        \\var x = adding(3, 4);
        \\print(x);
    ;
    var scanner = Scanner.init(source, allocator);
    defer scanner.deinit();
    try scanner.scan();
    const expected_tokens = [_]TokenType{
        // Function definition
        TokenType.Fun,
        TokenType.Identifier,
        TokenType.LeftParen,
        TokenType.Identifier,
        TokenType.Comma,
        TokenType.Identifier,
        TokenType.RightParen,
        TokenType.LeftBrace,
        TokenType.Return,
        TokenType.Identifier,
        TokenType.Plus,
        TokenType.Identifier,
        TokenType.Semicolon,
        TokenType.RightBrace,
        // Variable declaration and function call
        TokenType.Var,
        TokenType.Identifier,
        TokenType.Equal,
        TokenType.Identifier,
        TokenType.LeftParen,
        TokenType.Number,
        TokenType.Comma,
        TokenType.Number,
        TokenType.RightParen,
        TokenType.Semicolon,
        // Print statement
        TokenType.Print,
        TokenType.LeftParen,
        TokenType.Identifier,
        TokenType.RightParen,
        TokenType.Semicolon,
        TokenType.Eof,
    };
    try testing.expectEqual(expected_tokens.len, scanner.tokens.items.len);
    for (scanner.tokens.items, expected_tokens) |token, expected_type| {
        try testing.expectEqual(expected_type, token.type);
    }
}
