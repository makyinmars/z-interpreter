const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("scanner.zig").Token;
const TokenType = @import("scanner.zig").TokenType;
const Expr = @import("ast.zig").Expr;
const Stmt = @import("ast.zig").Stmt;
const Ast = @import("ast.zig").Ast;
const Value = @import("ast.zig").Value;
const testing = std.testing;

pub const SemanticError = error{
    AnalysisFailed,
    OutOfMemory,
    VariableAlreadyDeclared,
    UndefinedVariable,
};

/// The SemanticAnalyzer struct is responsible for performing semantic analysis on the AST.
pub const SemanticAnalyzer = struct {
    allocator: Allocator, // Memory allocator for error messages
    scopes: std.ArrayList(std.StringHashMap(bool)), // Stack of scopes for variable tracking
    errors: std.ArrayList([]const u8), // List of semantic errors

    /// Initializes a new SemanticAnalyzer instance.
    ///
    /// Parameters:
    /// - `allocator`: The memory allocator for error messages and scopes.
    ///
    /// Returns:
    /// A new SemanticAnalyzer instance.
    pub fn init(allocator: Allocator) SemanticAnalyzer {
        return SemanticAnalyzer{
            .allocator = allocator,
            .scopes = std.ArrayList(std.StringHashMap(bool)).init(allocator),
            .errors = std.ArrayList([]const u8).init(allocator),
        };
    }

    /// Deinitializes the SemanticAnalyzer instance, freeing all allocated memory.
    pub fn deinit(self: *SemanticAnalyzer) void {
        for (self.scopes.items) |*scope| {
            scope.deinit();
        }
        self.scopes.deinit();
        for (self.errors.items) |err| {
            self.allocator.free(err);
        }
        self.errors.deinit();
    }

    /// Analyzes the AST for semantic correctness.
    ///
    /// Parameters:
    /// - `ast`: The AST to analyze.
    ///
    /// Returns:
    /// A list of semantic errors, if any.
    pub fn analyze(self: *SemanticAnalyzer, ast: *const Ast) SemanticError![]const []const u8 {

        // Create the global scope before analyzing
        try self.beginScope();
        defer self.endScope();
        for (ast.statements.items) |stmt| {
            try self.analyzeStmt(stmt);
        }
        return self.errors.items;
    }

    /// Analyzes a statement.
    fn analyzeStmt(self: *SemanticAnalyzer, stmt: *const Stmt) SemanticError!void {
        switch (stmt.*) {
            .Expression => |expr| try self.analyzeExpr(expr.expression),
            .Print => |print| try self.analyzeExpr(print.expression),
            .Var => |var_decl| {
                if (var_decl.initializer) |initializer| {
                    try self.analyzeExpr(initializer);
                }
                try self.declareVariable(var_decl.name);
                try self.defineVariable(var_decl.name);
            },
            .Block => |block| {
                try self.beginScope();
                for (block.statements.items) |inner_stmt| {
                    try self.analyzeStmt(inner_stmt);
                }
                defer self.endScope();
            },
            .If => |if_stmt| {
                try self.analyzeExpr(if_stmt.condition);
                try self.analyzeStmt(if_stmt.thenBranch);
                if (if_stmt.elseBranch) |else_branch| {
                    try self.analyzeStmt(else_branch);
                }
            },
            .While => |while_stmt| {
                try self.analyzeExpr(while_stmt.condition);
                try self.analyzeStmt(while_stmt.body);
            },
            .Function => |func| {
                try self.declareVariable(func.name);
                try self.defineVariable(func.name);
                try self.analyzeFunction(func.function);
            },
            .Return => |ret| {
                if (ret.value) |value| {
                    try self.analyzeExpr(value);
                }
            },
        }
    }

    /// Analyzes an expression.
    fn analyzeExpr(self: *SemanticAnalyzer, expr: *const Expr) SemanticError!void {
        switch (expr.*) {
            .Literal => |lit| {
                // Literal expressions have no further semantic checks
                _ = lit;
            },
            .Variable => |v| {
                if (!self.isVariableDefined(v.name.lexeme)) {
                    const msg = try std.fmt.allocPrint(self.allocator, "Undefined variable '{s}'", .{v.name.lexeme});
                    defer self.allocator.free(msg);
                    try self.reportError(v.name, msg);
                }
            },
            .Binary => |bin| {
                try self.analyzeExpr(bin.left);
                try self.analyzeExpr(bin.right);
            },
            .Unary => |un| {
                try self.analyzeExpr(un.right);
            },
            .Grouping => |group| {
                try self.analyzeExpr(group.expression);
            },
            .Assign => |assign| {
                try self.analyzeExpr(assign.value);
                if (!self.isVariableDefined(assign.name.lexeme)) {
                    const msg = try std.fmt.allocPrint(self.allocator, "Undefined variable '{s}'", .{assign.name.lexeme});
                    defer self.allocator.free(msg);
                    try self.reportError(assign.name, msg);
                }
            },
            .Logical => |log| {
                try self.analyzeExpr(log.left);
                try self.analyzeExpr(log.right);
            },
            .Call => |call| {
                try self.analyzeExpr(call.callee);
                for (call.arguments.items) |arg| {
                    try self.analyzeExpr(arg);
                }
            },
            .Function => |func| {
                try self.beginScope();
                for (func.params.items) |param| {
                    try self.declareVariable(param);
                    try self.defineVariable(param);
                }
                for (func.body.items) |stmt| {
                    try self.analyzeStmt(stmt);
                }
                defer self.endScope();
            },
        }
    }

    /// Analyzes a function expression.
    fn analyzeFunction(self: *SemanticAnalyzer, func: *const Expr) SemanticError!void {
        switch (func.*) {
            .Function => |f| {
                try self.beginScope();
                for (f.params.items) |param| {
                    try self.declareVariable(param);
                    try self.defineVariable(param);
                }
                for (f.body.items) |stmt| {
                    try self.analyzeStmt(stmt);
                }
                defer self.endScope();
            },
            else => unreachable, // This should never happen
        }
    }

    /// Begins a new scope.
    fn beginScope(self: *SemanticAnalyzer) SemanticError!void {
        const scope = std.StringHashMap(bool).init(self.allocator);
        try self.scopes.append(scope);
    }

    /// Ends the current scope.
    fn endScope(self: *SemanticAnalyzer) void {
        var scope = self.scopes.pop();
        scope.deinit();
    }

    /// Declares a variable in the current scope.
    fn declareVariable(self: *SemanticAnalyzer, name: Token) !void {
        if (self.scopes.items.len == 0) return;

        const scope = &self.scopes.items[self.scopes.items.len - 1];
        if (scope.contains(name.lexeme)) {
            const msg = try std.fmt.allocPrint(self.allocator, "Variable '{s}' already declared in this scope", .{name.lexeme});
            defer self.allocator.free(msg);
            try self.reportError(name, msg);
        }
        try scope.put(name.lexeme, false); // Mark as not yet defined
    }

    /// Defines a variable in the current scope.
    fn defineVariable(self: *SemanticAnalyzer, name: Token) !void {
        if (self.scopes.items.len == 0) return;

        const scope = &self.scopes.items[self.scopes.items.len - 1];
        try scope.put(name.lexeme, true); // Mark as defined
    }

    /// Checks if a variable is defined in any scope.
    fn isVariableDefined(self: *SemanticAnalyzer, name: []const u8) bool {
        for (self.scopes.items) |*scope| {
            if (scope.get(name)) |defined| {
                return defined;
            }
        }
        return false;
    }

    /// Reports a semantic error.
    fn reportError(self: *SemanticAnalyzer, token: Token, message: []const u8) !void {
        const err_msg = try std.fmt.allocPrint(self.allocator, "[line {}] Semantic error at '{s}': {s}.", .{ token.line, token.lexeme, message });
        try self.errors.append(err_msg);
    }
};

// Explanation of Tests
// 1. **Variable Declaration and Definition**: This test checks that a variable can be declared and defined without errors.
// 2. **Undefined Variable**: This test ensures that an error is reported when an undefined variable is used.
// 3. **Scope Management**: This test verifies that variables can be declared in nested scopes without conflicts.
// 4. **Function Declaration and Scope**: This test checks that functions can be declared and that their parameters and body are correctly analyzed within their own scope.

test "SemanticAnalyzer: variable declaration and definition" {
    const allocator = testing.allocator;
    var analyzer = SemanticAnalyzer.init(allocator);
    defer analyzer.deinit();

    // Create a simple AST with a variable declaration
    var ast = Ast{
        .allocator = allocator,
        .statements = std.ArrayList(*Stmt).init(allocator),
    };
    defer ast.statements.deinit();

    const var_decl = try allocator.create(Stmt);
    errdefer allocator.destroy(var_decl);

    var_decl.* = Stmt{ .Var = .{
        .name = Token{ .type = TokenType.Identifier, .lexeme = "x", .line = 1 },
        .initializer = null,
    } };
    try ast.statements.append(var_decl);
    defer allocator.destroy(var_decl);

    // Analyze the AST
    const errors = try analyzer.analyze(&ast);
    try testing.expectEqual(@as(usize, 0), errors.len);
}
//
test "SemanticAnalyzer: undefined variable" {
    const allocator = testing.allocator;
    var analyzer = SemanticAnalyzer.init(allocator);
    defer analyzer.deinit();

    // Create a simple AST with an undefined variable
    var ast = Ast{
        .allocator = allocator,
        .statements = std.ArrayList(*Stmt).init(allocator),
    };
    defer ast.statements.deinit();

    const expr = try allocator.create(Expr);
    errdefer allocator.destroy(expr);
    expr.* = Expr{ .Variable = .{
        .name = Token{ .type = TokenType.Identifier, .lexeme = "x", .line = 1 },
    } };

    const stmt = try allocator.create(Stmt);
    errdefer allocator.destroy(stmt);
    stmt.* = Stmt{ .Expression = .{ .expression = expr } };

    try ast.statements.append(stmt);
    defer allocator.destroy(stmt);
    defer allocator.destroy(expr);

    // Analyze the AST
    const errors = try analyzer.analyze(&ast);
    try testing.expectEqual(@as(usize, 1), errors.len);
    try testing.expectEqualStrings("[line 1] Semantic error at 'x': Undefined variable 'x'.", errors[0]);
}

test "SemanticAnalyzer: scope management" {
    const allocator = testing.allocator;
    var analyzer = SemanticAnalyzer.init(allocator);
    defer analyzer.deinit();

    // Create a simple AST with nested scopes
    var ast = Ast{
        .allocator = allocator,
        .statements = std.ArrayList(*Stmt).init(allocator),
    };
    defer ast.statements.deinit();

    const inner_var_decl = try allocator.create(Stmt);
    errdefer allocator.destroy(inner_var_decl);
    defer allocator.destroy(inner_var_decl);
    inner_var_decl.* = Stmt{ .Var = .{
        .name = Token{ .type = TokenType.Identifier, .lexeme = "x", .line = 1 },
        .initializer = null,
    } };

    const block = try allocator.create(Stmt);
    errdefer allocator.destroy(block);
    defer allocator.destroy(block);
    block.* = Stmt{ .Block = .{
        .statements = std.ArrayList(*Stmt).init(allocator),
    } };
    defer block.Block.statements.deinit();

    try block.Block.statements.append(inner_var_decl);

    const outer_var_decl = try allocator.create(Stmt);
    errdefer allocator.destroy(outer_var_decl);
    defer allocator.destroy(outer_var_decl);
    outer_var_decl.* = Stmt{ .Var = .{
        .name = Token{ .type = TokenType.Identifier, .lexeme = "x", .line = 1 },
        .initializer = null,
    } };

    try ast.statements.append(outer_var_decl);
    try ast.statements.append(block);

    // Analyze the AST
    const errors = try analyzer.analyze(&ast);
    try testing.expectEqual(@as(usize, 0), errors.len);
}

test "SemanticAnalyzer: function declaration and scope" {
    const allocator = testing.allocator;
    var analyzer = SemanticAnalyzer.init(allocator);
    defer analyzer.deinit();

    // Create a simple AST with a function declaration
    var ast = Ast{
        .allocator = allocator,
        .statements = std.ArrayList(*Stmt).init(allocator),
    };
    defer ast.statements.deinit();

    const func = try allocator.create(Expr);
    defer allocator.destroy(func);

    func.* = Expr{ .Function = .{
        .params = std.ArrayList(Token).init(allocator),
        .body = std.ArrayList(*Stmt).init(allocator),
    } };

    const func_decl = try allocator.create(Stmt);
    defer allocator.destroy(func_decl);
    func_decl.* = Stmt{ .Function = .{
        .name = Token{ .type = TokenType.Identifier, .lexeme = "foo", .line = 1 },
        .function = func,
    } };

    try ast.statements.append(func_decl);

    // Analyze the AST
    const errors = try analyzer.analyze(&ast);
    try testing.expectEqual(@as(usize, 0), errors.len);
}
