// Main Scanner Logic
// - Empty file
// - Parentheses
// - Braces
// - Other single-character tokens
// - Lexical errors
// - Assignment & equality Operators
// - Negation & inequality operators
// - Relational operators
// - Division operator & comments
// - Whitespace
// - Multi-line errors
// - String literals
// - Number literals
// - Identifiers
// - Reserved words
const std = @import("std");

// Define a TokenType for all token types (e.g: Identifier, Number, String, etc)
const TokenType = enum {
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
    Super,
    This,
    True,
    Var,
    While,
    Error,
    Eof,
};

// Create a token struct to hold token type and value
const Token = struct {
    type: TokenType, // The type of the token (e.g., Identifier, Number, String, etc.)
    lexeme: []const u8, // The actual text of the token as it appears in the source code
    line: usize, // The line number in the source code where the token appears
};

// Create a Scanner struct with methods to scan the input
pub const Scanner = struct {
    source: []const u8, // The complete source code to scan
    start: usize = 0, // Starting position of current lexeme
    current: usize = 0, // Current position in source
    line: usize = 1, // Current line number
    tokens: std.ArrayList(Token), // List of tokens found
    allocator: std.mem.Allocator, // Memory allocator for strings/tokens

    pub fn init(source: []const u8, allocator: std.mem.Allocator) Scanner {
        return Scanner{
            .source = source,
            .tokens = std.ArrayList(Token).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Scanner) void {
        self.tokens.deinit();
    }

    // Scan the entire source code and tokenize it.
    // This function processes the source code character by character,
    // identifies tokens, and adds them to the tokens list.
    // It also handles the end of the input by adding an EOF token.
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

    // Helper method to check if we've reached the end of the source code
    fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.source.len;
    }

    // scanToken(): Scan a single token
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
            return error.UnterminatedString;
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

    // Add an error token to the tokens list
    // This is used to report lexical errors while still maintaining
    // the ability to continue scanning the rest of the source
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

    // Advance the scanner by one character and return it
    // This function moves the current position forward and returns the character
    // at the previous position. It's used to consume characters from the source.
    fn advance(self: *Scanner) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }

    // Check if the next character matches the expected character
    // If it matches, advance and return true. Otherwise, return false.
    // This is used for two-character tokens like !=, ==, etc.
    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;

        self.current += 1;
        return true;
    }

    // Look at the current character without consuming it
    // Returns the character at the current position or 0 if at end of input
    // This is used for lookahead without advancing the scanner
    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    // Handle multi-line comments (/* ... */) and single-line comments (// ...)
    // This function scans until it finds the closing */ sequence for multi-line comments
    // or the end of the line for single-line comments.
    // It handles nested comments and tracks line numbers.
    // Returns an error if the multi-line comment is unterminated.
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

    // Look at the next character without consuming it
    // Similar to peek() but looks one character ahead
    // Returns the character at current+1 position or 0 if at end of input
    fn peekNext(self: *Scanner) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }

    // This function scans an identifier or a reserved word from the source code.
    // It starts at the current position and continues until it encounters a character
    // that is not part of an identifier (e.g., whitespace, punctuation, etc.).
    // After scanning the identifier, it checks if it matches any reserved words.
    // If it does, it adds the corresponding reserved word token; otherwise, it adds
    // an identifier token.
    fn scanIdentifier(self: *Scanner) !void {
        // Continue scanning while the current character is a letter, digit, or underscore
        while (isAlphaNumeric(self.peek())) {
            _ = self.advance();
        }

        // Extract the lexeme from the source code
        const lexeme = self.source[self.start..self.current];

        // Check if the lexeme matches any reserved words
        const tokenType = if (std.mem.eql(u8, lexeme, "and")) TokenType.And else if (std.mem.eql(u8, lexeme, "else")) TokenType.Else else if (std.mem.eql(u8, lexeme, "false")) TokenType.False else if (std.mem.eql(u8, lexeme, "for")) TokenType.For else if (std.mem.eql(u8, lexeme, "fun")) TokenType.Fun else if (std.mem.eql(u8, lexeme, "if")) TokenType.If else if (std.mem.eql(u8, lexeme, "nil")) TokenType.Nil else if (std.mem.eql(u8, lexeme, "or")) TokenType.Or else if (std.mem.eql(u8, lexeme, "print")) TokenType.Print else if (std.mem.eql(u8, lexeme, "return")) TokenType.Return else if (std.mem.eql(u8, lexeme, "super")) TokenType.Super else if (std.mem.eql(u8, lexeme, "this")) TokenType.This else if (std.mem.eql(u8, lexeme, "true")) TokenType.True else if (std.mem.eql(u8, lexeme, "var")) TokenType.Var else if (std.mem.eql(u8, lexeme, "while")) TokenType.While else TokenType.Identifier;

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

test "Scanner Test" {

    // 1. **Source Code**: The `source` string contains a simple program with variable declaration, an `if` statement, and a `print` statement. This will generate a variety of tokens for testing.
    // 2. **Scanner Initialization**: The `Scanner` is initialized with the source code and an allocator. The `defer scanner.deinit()` ensures that the scanner's resources are properly cleaned up after the test.
    // 3. **Token Scanning**: The `scanner.scan()` method is called to tokenize the source code.
    // 4. **Expected Tokens**: The `expected_tokens` array contains the expected sequence of `TokenType` values that should be generated by the scanner.
    // 5. **Token Verification**: The test checks that the number of tokens generated matches the number of expected tokens. Then, it iterates through the generated tokens and verifies that each token's type matches the corresponding expected type.
    // 6. **Assertions**: The `std.testing.expectEqual` function is used to assert that the actual tokens match the expected tokens.

    // Initialize the allocator (e.g., using the testing allocator)
    const allocator = std.testing.allocator;

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
    try std.testing.expectEqual(expected_tokens.len, scanner.tokens.items.len);

    // Verify each token's type matches the expected type
    for (scanner.tokens.items, expected_tokens) |token, expected_type| {
        try std.testing.expectEqual(expected_type, token.type);
    }
}
