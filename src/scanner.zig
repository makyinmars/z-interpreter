const std = @import("std");
const testing = std.testing;

// Define a TokenType for all token types (e.g: Identifier, Number, String, etc)
pub const TokenType = enum {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier,
    String,
    Number,
    And,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    True,
    Var,
    While,
    Error,
    Eof,
};

pub const Token = struct {
    type: TokenType, // The type of the token (e.g., Identifier, Number, String, etc.)
    lexeme: []const u8, // The actual text of the token as it appears in the source code
    line: usize, // The line number in the source code where the token appears
};

pub const Scanner = struct {
    source: []const u8, // The complete source code to scan
    start: usize = 0, // Starting position of current lexeme
    current: usize = 0, // Current position in source
    line: usize = 1, // Current line number
    tokens: std.ArrayList(Token), // List of tokens found
    allocator: std.mem.Allocator, // Memory allocator for strings/tokens

    /// Initializes a new Scanner instance.
    ///
    /// Parameters:
    /// - `source`: The source code to be scanned.
    /// - `allocator`: The memory allocator to be used for token storage.
    ///
    /// Returns:
    /// A new Scanner instance with the provided source and allocator.
    pub fn init(source: []const u8, allocator: std.mem.Allocator) Scanner {
        return Scanner{
            .source = source,
            .tokens = std.ArrayList(Token).init(allocator),
            .allocator = allocator,
        };
    }

    /// Deinitializes the Scanner instance, freeing all allocated memory.
    /// This function should be called when the scanner is no longer needed.
    /// It frees any dynamically allocated memory, including error tokens and the tokens list.
    /// Note: String literals are not freed as they are slices of the original source.
    pub fn deinit(self: *Scanner) void {
        for (self.tokens.items) |token| {
            // Only free error messages (which we allocated with allocPrint)
            if (token.type == TokenType.Error) {
                self.allocator.free(token.lexeme);
            }
            // For strings, we only need to free if we copied them
            // (currently we don't copy string literals, we just slice them)
        }
        self.tokens.deinit();
    }

    /// This function processes the source code character by character,
    /// identifies tokens, and adds them to the tokens list.
    /// It also handles the end of the input by adding an EOF token.
    pub fn scan(self: *Scanner) !void {
        // Loop through the source code until we reach the end
        while (!self.isAtEnd()) {
            // Set the start position of the current lexeme
            self.start = self.current;
            // Scan a single token and add it to the tokens list
            try self.scanToken();
        }

        // Add an EOF token to signify the end of the input
        try self.tokens.append(Token{
            .type = TokenType.Eof,
            .lexeme = self.source[self.start..self.current],
            .line = self.line,
        });
    }

    /// Helper method to check if we've reached the end of the source code
    fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.source.len;
    }

    /// This function scans a single token from the source code.
    /// It processes the current character and determines the appropriate token type.
    /// It handles single-character tokens, multi-character tokens, strings, numbers,
    /// identifiers, and comments. It also tracks line numbers and handles errors.
    fn scanToken(self: *Scanner) !void {
        const c = self.advance();

        switch (c) {
            '(' => try self.addToken(TokenType.LeftParen),
            ')' => try self.addToken(TokenType.RightParen),
            '{' => try self.addToken(TokenType.LeftBrace),
            '}' => try self.addToken(TokenType.RightBrace),
            ',' => try self.addToken(TokenType.Comma),
            '.' => try self.addToken(TokenType.Dot),
            '-' => try self.addToken(TokenType.Minus),
            '+' => try self.addToken(TokenType.Plus),
            ';' => try self.addToken(TokenType.Semicolon),
            '*' => try self.addToken(TokenType.Star),
            '!' => {
                const tokenType = if (self.match('=')) TokenType.BangEqual else TokenType.Bang;
                try self.addToken(tokenType);
            },
            '=' => {
                const tokenType = if (self.match('=')) TokenType.EqualEqual else TokenType.Equal;
                try self.addToken(tokenType);
            },
            '<' => {
                const tokenType = if (self.match('=')) TokenType.LessEqual else TokenType.Less;
                try self.addToken(tokenType);
            },
            '>' => {
                const tokenType = if (self.match('=')) TokenType.GreaterEqual else TokenType.Greater;
                try self.addToken(tokenType);
            },
            '/' => {
                if (self.match('/')) {
                    // Handle single-line comments
                    while (self.peek() != '\n' and !self.isAtEnd()) {
                        _ = self.advance();
                    }
                } else if (self.match('*')) {
                    // Handle multi-line comments
                    try self.handleMultiLineComment();
                } else {
                    try self.addToken(TokenType.Slash);
                }
            },
            ' ', '\r', '\t' => {
                // Skip whitespace
            },
            '\n' => {
                self.line += 1;
            },
            '"' => try self.scanString(),
            '0'...'9' => try self.scanNumber(),
            'a'...'z', 'A'...'Z', '_' => try self.scanIdentifier(),
            else => {
                // Handle unexpected characters
                try self.addErrorToken("Unexpected character.");
            },
        }
    }

    fn isAlphaNumeric(c: u8) bool {
        return switch (c) {
            'a'...'z', 'A'...'Z', '0'...'9', '_' => true,
            else => false,
        };
    }

    // Scan a string literal from the source code
    // This function handles string literals enclosed in double quotes
    // It supports multi-line strings and tracks line numbers
    // Returns an error if the string is unterminated
    fn scanString(self: *Scanner) !void {
        // Continue until we find the closing quote or reach end of input
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1; // Track newlines in multi-line strings
            }
            _ = self.advance();
        }

        // If we reached end of input without finding closing quote
        if (self.isAtEnd()) {
            try self.addErrorToken("Undertinated string");
            return;
        }

        // Consume the closing quote
        _ = self.advance();

        // Add the string token (excluding the quotes)
        try self.addToken(TokenType.String);
    }

    // Scan a number literal from the source code
    // This function handles both integer and floating-point numbers
    // It supports numbers with decimal points but no scientific notation
    fn scanNumber(self: *Scanner) !void {
        // Consume all digits before decimal point
        while (isDigit(self.peek())) {
            _ = self.advance();
        }

        // Look for fractional part
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            // Consume the decimal point
            _ = self.advance();

            // Consume digits after decimal point
            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }

        // Add the number token
        try self.addToken(TokenType.Number);
    }

    // Helper function to check if a character is a digit
    fn isDigit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    /// This is used to report lexical errors while still maintaining
    /// the ability to continue scanning the rest of the source
    fn addErrorToken(self: *Scanner, message: []const u8) !void {
        const lexeme = self.source[self.start..self.current];
        // Create a new string that combines the lexeme and error message
        const error_lexeme = try std.fmt.allocPrint(self.allocator, "{s} ({s})", .{ lexeme, message });
        try self.tokens.append(Token{
            .type = TokenType.Error,
            .lexeme = error_lexeme,
            .line = self.line,
        });
    }

    /// Advance the scanner by one character and return it
    /// This function moves the current position forward and returns the character
    /// at the previous position. It's used to consume characters from the source.
    fn advance(self: *Scanner) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    /// Check if the next character matches the expected character
    /// If it matches, advance and return true. Otherwise, return false.
    /// This is used for two-character tokens like !=, ==, etc.
    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;

        self.current += 1;
        return true;
    }

    /// Look at the current character without consuming it
    /// Returns the character at the current position or 0 if at end of input
    /// This is used for lookahead without advancing the scanner
    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    /// Handle multi-line comments (/* ... */) and single-line comments (// ...)
    /// This function scans until it finds the closing */ sequence for multi-line comments
    /// or the end of the line for single-line comments.
    /// It handles nested comments and tracks line numbers.
    /// Returns an error if the multi-line comment is unterminated.
    fn handleMultiLineComment(self: *Scanner) !void {
        var depth: usize = 1;
        while (depth > 0 and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }

            if (self.peek() == '/' and self.peekNext() == '*') {
                depth += 1;
                _ = self.advance();
                _ = self.advance();
            } else if (self.peek() == '*' and self.peekNext() == '/') {
                depth -= 1;
                _ = self.advance();
                _ = self.advance();
            } else if (self.peek() == '/' and self.peekNext() == '/') {
                // Handle single-line comments within multi-line comments
                while (self.peek() != '\n' and !self.isAtEnd()) {
                    _ = self.advance();
                }
            } else {
                _ = self.advance();
            }
        }

        if (depth > 0) {
            return error.UnterminatedComment;
        }
    }

    /// Look at the next character without consuming it
    /// Similar to peek() but looks one character ahead
    /// Returns the character at current+1 position or 0 if at end of input
    fn peekNext(self: *Scanner) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }

    /// This function scans an identifier or a reserved word from the source code.
    /// It starts at the current position and continues until it encounters a character
    /// that is not part of an identifier (e.g., whitespace, punctuation, etc.).
    /// After scanning the identifier, it checks if it matches any reserved words.
    /// If it does, it adds the corresponding reserved word token; otherwise, it adds
    /// an identifier token.
    fn scanIdentifier(self: *Scanner) !void {
        // Continue scanning while the current character is a letter, digit, or underscore
        while (isAlphaNumeric(self.peek())) {
            _ = self.advance();
        }

        // Extract the lexeme from the source code
        const lexeme = self.source[self.start..self.current];

        // Check if the lexeme matches any reserved words
        const tokenType = if (std.mem.eql(u8, lexeme, "and")) TokenType.And else if (std.mem.eql(u8, lexeme, "else")) TokenType.Else else if (std.mem.eql(u8, lexeme, "false")) TokenType.False else if (std.mem.eql(u8, lexeme, "for")) TokenType.For else if (std.mem.eql(u8, lexeme, "fun")) TokenType.Fun else if (std.mem.eql(u8, lexeme, "if")) TokenType.If else if (std.mem.eql(u8, lexeme, "nil")) TokenType.Nil else if (std.mem.eql(u8, lexeme, "or")) TokenType.Or else if (std.mem.eql(u8, lexeme, "print")) TokenType.Print else if (std.mem.eql(u8, lexeme, "return")) TokenType.Return else if (std.mem.eql(u8, lexeme, "true")) TokenType.True else if (std.mem.eql(u8, lexeme, "var")) TokenType.Var else if (std.mem.eql(u8, lexeme, "while")) TokenType.While else TokenType.Identifier;

        // Add the token to the tokens list
        try self.addToken(tokenType);
    }

    // Add a token to the tokens list with the current lexeme
    // This function creates a new token using the current scanner state:
    // - type: The token type to add
    // - lexeme: The text from start to current position in source
    // - line: The current line number
    // The token is then appended to the tokens list
    fn addToken(self: *Scanner, tokenType: TokenType) !void {
        const lexeme = self.source[self.start..self.current];
        try self.tokens.append(Token{
            .type = tokenType,
            .lexeme = lexeme,
            .line = self.line,
        });
    }

    // Skip whitespace characters (spaces, tabs, carriage returns)
    // This function advances the scanner past any whitespace characters
    // without adding them as tokens. It handles spaces (' '), tabs ('\t'),
    // and carriage returns ('\r'). Newlines are handled separately as they
    // increment the line counter.
    fn skipWhitespace(self: *Scanner) void {
        while (true) {
            switch (self.peek()) {
                ' ', '\r', '\t' => _ = self.advance(),
                else => break,
            }
        }
    }

    // This function is used to report lexical errors while still maintaining
    // the ability to continue scanning the rest of the source
    fn handleErrors(self: *Scanner, message: []const u8) !void {
        try self.addErrorToken(message);
    }
};

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
