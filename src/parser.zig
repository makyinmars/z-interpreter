const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("scanner.zig").Token;
const TokenType = @import("scanner.zig").TokenType;
const Expr = @import("ast.zig").Expr;
const Stmt = @import("ast.zig").Stmt;
const Ast = @import("ast.zig").Ast;
const testing = std.testing;

pub const ParseError = error{
    ParseError,
    OutOfMemory,
};

/// The Parser struct is responsible for parsing a sequence of tokens into an AST.
pub const Parser = struct {
    tokens: []const Token, // The list of tokens to parse
    current: usize = 0, // The current position in the tokens list
    allocator: Allocator, // The memory allocator for AST nodes
    ast: Ast, // The resulting AST

    /// Initializes a new Parser instance.
    ///
    /// Parameters:
    /// - `tokens`: The list of tokens to parse.
    /// - `allocator`: The memory allocator for AST nodes.
    ///
    /// Returns:
    /// A new Parser instance.
    pub fn init(tokens: []const Token, allocator: Allocator) Parser {
        return Parser{
            .tokens = tokens,
            .allocator = allocator,
            .ast = Ast.init(allocator),
        };
    }

    /// Deinitializes the Parser instance, freeing all allocated memory.
    /// This function should be called when the parser is no longer needed.
    pub fn deinit(self: *Parser) void {
        self.ast.deinit();
    }

    /// Parses the tokens into an AST.
    ///
    /// Returns:
    /// The parsed AST.
    pub fn parse(self: *Parser) ParseError!Ast {
        while (!self.isAtEnd()) {
            const stmt = try self.declaration();
            try self.ast.statements.append(stmt);
        }
        return self.ast;
    }

    /// Parses a declaration, which can be a variable declaration or a statement.
    fn declaration(self: *Parser) ParseError!*Stmt {
        if (self.match(.Var)) {
            return try self.varDeclaration();
        }
        return try self.statement();
    }

    /// Parses a variable declaration.
    fn varDeclaration(self: *Parser) ParseError!*Stmt {
        const name = try self.consume(.Identifier, "Expected variable name.");
        var initializer: ?*Expr = null;
        if (self.match(.Equal)) {
            initializer = try self.expression();
        }
        _ = try self.consume(.Semicolon, "Expected ';' after variable declaration.");
        const stmt = try self.allocator.create(Stmt);
        stmt.* = .{ .Var = .{ .name = name, .initializer = initializer } };
        return stmt;
    }

    /// Parses a statement, which can be an expression, print, block, if, while, or return statement.
    fn statement(self: *Parser) ParseError!*Stmt {
        if (self.match(.Print)) {
            return try self.printStatement();
        }
        if (self.match(.LeftBrace)) {
            return try self.blockStatement();
        }
        if (self.match(.If)) {
            return try self.ifStatement();
        }
        if (self.match(.While)) {
            return try self.whileStatement();
        }
        if (self.match(.Return)) {
            return try self.returnStatement();
        }
        return try self.expressionStatement();
    }

    /// Parses a print statement.
    fn printStatement(self: *Parser) ParseError!*Stmt {
        const expr = try self.expression();
        _ = try self.consume(.Semicolon, "Expected ';' after value.");
        const stmt = try self.allocator.create(Stmt);
        stmt.* = .{ .Print = .{ .expression = expr } };
        return stmt;
    }

    /// Parses a block statement.
    fn blockStatement(self: *Parser) ParseError!*Stmt {
        var statements = std.ArrayList(*Stmt).init(self.allocator);
        while (!self.check(.RightBrace) and !self.isAtEnd()) {
            const stmt = try self.declaration();
            try statements.append(stmt);
        }
        _ = try self.consume(.RightBrace, "Expected '}' after block.");
        const stmt = try self.allocator.create(Stmt);
        stmt.* = .{ .Block = .{ .statements = statements } };
        return stmt;
    }

    /// Parses an if statement.
    fn ifStatement(self: *Parser) ParseError!*Stmt {
        _ = try self.consume(.LeftParen, "Expected '(' after 'if'.");
        const condition = try self.expression();
        _ = try self.consume(.RightParen, "Expected ')' after if condition.");

        const thenBranch = try self.statement();
        var elseBranch: ?*Stmt = null;
        if (self.match(.Else)) {
            elseBranch = try self.statement();
        }

        const stmt = try self.allocator.create(Stmt);
        stmt.* = .{ .If = .{ .condition = condition, .thenBranch = thenBranch, .elseBranch = elseBranch } };
        return stmt;
    }

    /// Parses a while statement.
    fn whileStatement(self: *Parser) ParseError!*Stmt {
        _ = try self.consume(.LeftParen, "Expected '(' after 'while'.");
        const condition = try self.expression();
        _ = try self.consume(.RightParen, "Expected ')' after condition.");
        const body = try self.statement();

        const stmt = try self.allocator.create(Stmt);
        stmt.* = .{ .While = .{ .condition = condition, .body = body } };
        return stmt;
    }

    /// Parses a return statement.
    fn returnStatement(self: *Parser) ParseError!*Stmt {
        const keyword = self.previous();
        var value: ?*Expr = null;
        if (!self.check(.Semicolon)) {
            value = try self.expression();
        }
        _ = try self.consume(.Semicolon, "Expected ';' after return value.");
        const stmt = try self.allocator.create(Stmt);
        stmt.* = .{ .Return = .{ .keyword = keyword, .value = value } };
        return stmt;
    }

    /// Parses an expression statement.
    fn expressionStatement(self: *Parser) ParseError!*Stmt {
        const expr = try self.expression();
        _ = try self.consume(.Semicolon, "Expected ';' after expression.");
        const stmt = try self.allocator.create(Stmt);
        stmt.* = .{ .Expression = .{ .expression = expr } };
        return stmt;
    }

    /// Parses an expression.
    fn expression(self: *Parser) ParseError!*Expr {
        return try self.assignment();
    }

    /// Parses an assignment expression.
    fn assignment(self: *Parser) ParseError!*Expr {
        const expr = try self.OR();

        if (self.match(.Equal)) {
            const equals = self.previous();
            const value = try self.assignment();

            if (expr.* == .Variable) {
                const name = expr.Variable.name;
                const assign_expr = try self.allocator.create(Expr);
                assign_expr.* = .{ .Assign = .{ .name = name, .value = value } };
                return assign_expr;
            }

            return self.err(equals, "Invalid assignment target.");
        }

        return expr;
    }

    /// Parses a logical OR expression.
    fn OR(self: *Parser) ParseError!*Expr {
        var expr = try self.AND();

        while (self.match(.Or)) {
            const operator = self.previous();
            const right = try self.AND();
            const logical_expr = try self.allocator.create(Expr);
            logical_expr.* = .{ .Logical = .{ .left = expr, .operator = operator, .right = right } };
            expr = logical_expr;
        }

        return expr;
    }

    /// Parses a logical AND expression.
    fn AND(self: *Parser) ParseError!*Expr {
        var expr = try self.equality();

        while (self.match(.And)) {
            const operator = self.previous();
            const right = try self.equality();
            const logical_expr = try self.allocator.create(Expr);
            logical_expr.* = .{ .Logical = .{ .left = expr, .operator = operator, .right = right } };
            expr = logical_expr;
        }

        return expr;
    }

    /// Parses an equality expression.
    fn equality(self: *Parser) ParseError!*Expr {
        var expr = try self.comparison();

        while (self.match(.BangEqual) or self.match(.EqualEqual)) {
            const operator = self.previous();
            const right = try self.comparison();
            const binary_expr = try self.allocator.create(Expr);
            binary_expr.* = .{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = binary_expr;
        }

        return expr;
    }

    /// Parses a comparison expression.
    fn comparison(self: *Parser) ParseError!*Expr {
        var expr = try self.term();

        while (self.match(.Greater) or self.match(.GreaterEqual) or self.match(.Less) or self.match(.LessEqual)) {
            const operator = self.previous();
            const right = try self.term();
            const binary_expr = try self.allocator.create(Expr);
            binary_expr.* = .{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = binary_expr;
        }

        return expr;
    }

    /// Parses a term expression (addition or subtraction).
    fn term(self: *Parser) ParseError!*Expr {
        var expr = try self.factor();

        while (self.match(.Plus) or self.match(.Minus)) {
            const operator = self.previous();
            const right = try self.factor();
            const binary_expr = try self.allocator.create(Expr);
            binary_expr.* = .{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = binary_expr;
        }

        return expr;
    }

    /// Parses a factor expression (multiplication or division).
    fn factor(self: *Parser) ParseError!*Expr {
        var expr = try self.unary();

        while (self.match(.Star) or self.match(.Slash)) {
            const operator = self.previous();
            const right = try self.unary();
            const binary_expr = try self.allocator.create(Expr);
            binary_expr.* = .{ .Binary = .{ .left = expr, .operator = operator, .right = right } };
            expr = binary_expr;
        }

        return expr;
    }

    /// Parses a unary expression.
    fn unary(self: *Parser) ParseError!*Expr {
        if (self.match(.Bang) or self.match(.Minus)) {
            const operator = self.previous();
            const right = try self.unary();
            const unary_expr = try self.allocator.create(Expr);
            unary_expr.* = .{ .Unary = .{ .operator = operator, .right = right } };
            return unary_expr;
        }

        return try self.primary();
    }

    /// Parses a primary expression (literals, variables, grouping, etc.).
    fn primary(self: *Parser) ParseError!*Expr {
        if (self.match(.False)) {
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .Literal = .{ .value = .{ .Bool = false } } };
            return expr;
        }
        if (self.match(.True)) {
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .Literal = .{ .value = .{ .Bool = true } } };
            return expr;
        }
        if (self.match(.Nil)) {
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .Literal = .{ .value = .{ .Nil = {} } } };
            return expr;
        }
        if (self.match(.Number)) {
            const value = std.fmt.parseFloat(f64, self.previous().lexeme) catch unreachable;
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .Literal = .{ .value = .{ .Number = value } } };
            return expr;
        }
        if (self.match(.String)) {
            const value = self.previous().lexeme;
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .Literal = .{ .value = .{ .String = value } } };
            return expr;
        }
        if (self.match(.Identifier)) {
            const expr = try self.allocator.create(Expr);
            expr.* = .{ .Variable = .{ .name = self.previous() } };
            return expr;
        }
        if (self.match(.LeftParen)) {
            const expr = try self.expression();
            _ = try self.consume(.RightParen, "Expected ')' after expression.");
            const grouping_expr = try self.allocator.create(Expr);
            grouping_expr.* = .{ .Grouping = .{ .expression = expr } };
            return grouping_expr;
        }

        return self.err(self.peek(), "Expected expression.");
    }

    /// Checks if the current token matches any of the given types.
    fn match(self: *Parser, token_type: TokenType) bool {
        if (self.check(token_type)) {
            _ = self.advance();
            return true;
        }

        return false;
    }

    /// Consumes the current token if it matches the expected type, otherwise reports an error.
    fn consume(self: *Parser, expected: TokenType, message: []const u8) !Token {
        if (self.check(expected)) {
            return self.advance();
        }
        return self.err(self.peek(), message);
    }

    /// Checks if the current token matches the expected type.
    fn check(self: *Parser, expected: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().type == expected;
    }

    /// Advances to the next token and returns the previous one.
    fn advance(self: *Parser) Token {
        if (!self.isAtEnd()) {
            self.current += 1;
        }
        return self.previous();
    }

    /// Checks if the parser has reached the end of the token list.
    fn isAtEnd(self: *Parser) bool {
        return self.peek().type == .Eof;
    }

    /// Returns the current token without consuming it.
    fn peek(self: *Parser) Token {
        return self.tokens[self.current];
    }

    /// Returns the previous token.
    fn previous(self: *Parser) Token {
        return self.tokens[self.current - 1];
    }

    /// Reports a syntax error with the given message.
    fn err(_: *Parser, token: Token, message: []const u8) error{ParseError} {
        std.debug.print("[line {}] Error at '{s}': {s}\n", .{ token.line, token.lexeme, message });
        return error.ParseError;
    }
};

test "Parser: variable declaration" {
    const allocator = testing.allocator;

    // Define a simple token sequence for a variable declaration
    const tokens = [_]Token{
        Token{ .type = TokenType.Var, .lexeme = "var", .line = 1 },
        Token{ .type = TokenType.Identifier, .lexeme = "x", .line = 1 },
        Token{ .type = TokenType.Equal, .lexeme = "=", .line = 1 },
        Token{ .type = TokenType.Number, .lexeme = "42", .line = 1 },
        Token{ .type = TokenType.Semicolon, .lexeme = ";", .line = 1 },
        Token{ .type = TokenType.Eof, .lexeme = "", .line = 1 },
    };

    var parser = Parser.init(tokens[0..], allocator);
    defer parser.deinit();

    const ast = try parser.parse();
    // Assert that the AST contains the expected variable declaration
    try std.testing.expect(ast.statements.items.len == 1);
    const stmt = ast.statements.items[0];
    try std.testing.expect(stmt.* == .Var);
    try std.testing.expectEqualStrings(stmt.Var.name.lexeme, "x");
    try std.testing.expect(stmt.Var.initializer != null);
    try std.testing.expect(stmt.Var.initializer.?.* == .Literal);
    try std.testing.expect(stmt.Var.initializer.?.Literal.value == .Number);
    try std.testing.expect(stmt.Var.initializer.?.Literal.value.Number == 42);
}

test "Parser: expression statement" {
    const allocator = testing.allocator;

    // Define a simple token sequence for an expression statement
    const tokens = [_]Token{
        Token{ .type = TokenType.Number, .lexeme = "42", .line = 1 },
        Token{ .type = TokenType.Semicolon, .lexeme = ";", .line = 1 },
        Token{ .type = TokenType.Eof, .lexeme = "", .line = 1 },
    };

    var parser = Parser.init(tokens[0..], allocator);
    defer parser.deinit();

    const ast = try parser.parse();
    // Assert that the AST contains the expected expression statement
    try std.testing.expect(ast.statements.items.len == 1);
    const stmt = ast.statements.items[0];
    try std.testing.expect(stmt.* == .Expression);
    try std.testing.expect(stmt.Expression.expression.* == .Literal);
    try std.testing.expect(stmt.Expression.expression.Literal.value == .Number);
    try std.testing.expect(stmt.Expression.expression.Literal.value.Number == 42);
}

test "Parser: if statement" {
    const allocator = testing.allocator;

    // Define a simple token sequence for an if statement
    const tokens = [_]Token{
        Token{ .type = TokenType.If, .lexeme = "if", .line = 1 },
        Token{ .type = TokenType.LeftParen, .lexeme = "(", .line = 1 },
        Token{ .type = TokenType.True, .lexeme = "true", .line = 1 },
        Token{ .type = TokenType.RightParen, .lexeme = ")", .line = 1 },
        Token{ .type = TokenType.LeftBrace, .lexeme = "{", .line = 1 },
        Token{ .type = TokenType.Print, .lexeme = "print", .line = 1 },
        Token{ .type = TokenType.Number, .lexeme = "42", .line = 1 },
        Token{ .type = TokenType.Semicolon, .lexeme = ";", .line = 1 },
        Token{ .type = TokenType.RightBrace, .lexeme = "}", .line = 1 },
        Token{ .type = TokenType.Eof, .lexeme = "", .line = 1 },
    };

    var parser = Parser.init(tokens[0..], allocator);
    defer parser.deinit();

    const ast = try parser.parse();
    // Assert that the AST contains the expected if statement
    try std.testing.expect(ast.statements.items.len == 1);
    const stmt = ast.statements.items[0];
    try std.testing.expect(stmt.* == .If);
    try std.testing.expect(stmt.If.condition.* == .Literal);
    try std.testing.expect(stmt.If.condition.Literal.value == .Bool);
    try std.testing.expect(stmt.If.condition.Literal.value.Bool == true);
    try std.testing.expect(stmt.If.thenBranch.* == .Block);
    try std.testing.expect(stmt.If.thenBranch.Block.statements.items.len == 1);
    try std.testing.expect(stmt.If.elseBranch == null);
}

test "Parser: while statement" {
    const allocator = testing.allocator;

    // Define a simple token sequence for a while statement
    const tokens = [_]Token{
        Token{ .type = TokenType.While, .lexeme = "while", .line = 1 },
        Token{ .type = TokenType.LeftParen, .lexeme = "(", .line = 1 },
        Token{ .type = TokenType.True, .lexeme = "true", .line = 1 },
        Token{ .type = TokenType.RightParen, .lexeme = ")", .line = 1 },
        Token{ .type = TokenType.LeftBrace, .lexeme = "{", .line = 1 },
        Token{ .type = TokenType.Print, .lexeme = "print", .line = 1 },
        Token{ .type = TokenType.Number, .lexeme = "42", .line = 1 },
        Token{ .type = TokenType.Semicolon, .lexeme = ";", .line = 1 },
        Token{ .type = TokenType.RightBrace, .lexeme = "}", .line = 1 },
        Token{ .type = TokenType.Eof, .lexeme = "", .line = 1 },
    };

    var parser = Parser.init(tokens[0..], allocator);
    defer parser.deinit();

    const ast = try parser.parse();
    // Assert that the AST contains the expected while statement
    try std.testing.expect(ast.statements.items.len == 1);
    const stmt = ast.statements.items[0];
    try std.testing.expect(stmt.* == .While);
    try std.testing.expect(stmt.While.condition.* == .Literal);
    try std.testing.expect(stmt.While.condition.Literal.value == .Bool);
    try std.testing.expect(stmt.While.condition.Literal.value.Bool == true);
    try std.testing.expect(stmt.While.body.* == .Block);
    try std.testing.expect(stmt.While.body.Block.statements.items.len == 1);
}
