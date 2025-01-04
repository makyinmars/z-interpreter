const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("scanner.zig").Token;
const scanner = @import("scanner.zig");
const ast = @import("ast.zig");

const stdout = std.io.getStdOut().writer();
const testing = std.testing;

pub const Expr = union(enum) {
    // Literal expressions
    Literal: struct {
        value: Value,
    },

    // Variable expressions
    Variable: struct {
        name: Token,
    },

    // Binary expressions
    Binary: struct {
        left: *Expr,
        operator: Token,
        right: *Expr,
    },

    // Unary expressions
    Unary: struct {
        operator: Token,
        right: *Expr,
    },

    // Grouping expressions
    Grouping: struct {
        expression: *Expr,
    },

    // Assignment expressions
    Assign: struct {
        name: Token,
        value: *Expr,
    },

    // Logical expressions
    Logical: struct {
        left: *Expr,
        operator: Token,
        right: *Expr,
    },

    // Call expressions
    Call: struct {
        callee: *Expr,
        paren: Token,
        arguments: std.ArrayList(*Expr),
    },

    // Function expressions
    Function: struct {
        params: std.ArrayList(Token),
        body: std.ArrayList(*Stmt),
    },
};

pub const Stmt = union(enum) {
    // Expression statements
    Expression: struct {
        expression: *Expr,
    },

    // Print statements
    Print: struct {
        expression: *Expr,
    },

    // Variable declarations
    Var: struct {
        name: Token,
        initializer: ?*Expr,
    },

    // Block statements
    Block: struct {
        statements: std.ArrayList(*Stmt),
    },

    // If statements
    If: struct {
        condition: *Expr,
        thenBranch: *Stmt,
        elseBranch: ?*Stmt,
    },

    // While statements
    While: struct {
        condition: *Expr,
        body: *Stmt,
    },

    // Function declarations
    Function: struct {
        name: Token,
        function: *Expr,
    },

    // Return statements
    Return: struct {
        keyword: Token,
        value: ?*Expr,
    },
};

pub const Value = union(enum) {
    Number: f64,
    String: []const u8,
    Bool: bool,
    Nil: void,
};

/// Represents an Abstract Syntax Tree (AST) for a programming language.
/// The AST is a tree structure that captures the syntactic structure of source code.
/// Each node in the tree represents a construct in the language, such as expressions or statements.
/// The AST is used for further processing, such as interpretation or compilation.
pub const Ast = struct {
    statements: std.ArrayList(*Stmt),
    allocator: Allocator,

    /// Initializes a new AST with the given allocator.
    /// The AST will use this allocator for all memory allocations.
    pub fn init(allocator: Allocator) Ast {
        return .{
            .statements = std.ArrayList(*Stmt).init(allocator),
            .allocator = allocator,
        };
    }

    /// Deinitializes the AST, freeing all memory associated with its statements and expressions.
    /// This recursively frees all child nodes and deallocates the memory used by the AST.
    pub fn deinit(self: *Ast) void {
        for (self.statements.items) |stmt| {
            self.freeStmt(stmt);
        }
        self.statements.deinit();
    }

    /// Recursively frees a statement and all its child nodes
    pub fn freeStmt(self: *Ast, stmt: *Stmt) void {
        switch (stmt.*) {
            .Expression => |expr| self.freeExpr(expr.expression),
            .Print => |print| self.freeExpr(print.expression),
            .Var => |v| {
                if (v.initializer) |initializer| {
                    self.freeExpr(initializer);
                }
            },

            .Block => |block| {
                for (block.statements.items) |inner_stmt| {
                    self.freeStmt(inner_stmt);
                }
                block.statements.deinit();
            },
            .If => |if_stmt| {
                self.freeExpr(if_stmt.condition);
                self.freeStmt(if_stmt.thenBranch);
                if (if_stmt.elseBranch) |else_branch| {
                    self.freeStmt(else_branch);
                }
            },
            .While => |while_stmt| {
                self.freeExpr(while_stmt.condition);
                self.freeStmt(while_stmt.body);
            },
            .Function => |func| {
                self.freeExpr(&func.function.*);
            },
            .Return => |ret| {
                if (ret.value) |value| {
                    self.freeExpr(value);
                }
            },
        }
        self.allocator.destroy(stmt);
    }

    /// Recursively frees an expression and all its child nodes
    pub fn freeExpr(self: *Ast, expr: *Expr) void {
        switch (expr.*) {
            // Literal and Variable nodes have no child nodes to free
            .Literal, .Variable => {},

            // Binary expressions have left and right child nodes
            .Binary => |bin| {
                self.freeExpr(bin.left);
                self.freeExpr(bin.right);
            },

            // Unary expressions have a single right child node
            .Unary => |un| self.freeExpr(un.right),

            // Grouping expressions wrap a single child expression
            .Grouping => |group| self.freeExpr(group.expression),

            // Assignment expressions contain a value expression
            .Assign => |assign| self.freeExpr(assign.value),

            // Logical expressions have left and right operands
            .Logical => |log| {
                self.freeExpr(log.left);
                self.freeExpr(log.right);
            },

            // Call expressions have a callee and arguments list
            .Call => |call| {
                self.freeExpr(call.callee);
                for (call.arguments.items) |arg| {
                    self.freeExpr(arg);
                }
                call.arguments.deinit();
            },

            // Function expressions contain parameter tokens and statement bodies
            .Function => |func| {
                for (func.body.items) |stmt| {
                    self.freeStmt(stmt);
                }
                func.body.deinit();
                func.params.deinit();
            },
        }
        self.allocator.destroy(expr);
    }

    // Helper function to print the AST with proper indentation
    pub fn printAst(tree: *const Ast, indent: usize) std.fs.File.WriteError!void {
        for (tree.statements.items) |stmt| {
            try printStmt(stmt, indent);
        }
    }

    // Print a statement with indentation
    fn printStmt(stmt: *ast.Stmt, indent: usize) std.fs.File.WriteError!void {
        try printIndent(indent);
        switch (stmt.*) {
            .Expression => |expr| {
                try stdout.print("Expression Statement:\n", .{});
                try printExpr(expr.expression, indent + 2);
            },
            .Print => |print| {
                try stdout.print("Print Statement:\n", .{});
                try printExpr(print.expression, indent + 2);
            },
            .Var => |var_stmt| {
                try stdout.print("Var Statement: {s}\n", .{var_stmt.name.lexeme});
                if (var_stmt.initializer) |initializer| {
                    try printIndent(indent + 2);
                    try stdout.print("Initializer:\n", .{});
                    try printExpr(initializer, indent + 4);
                }
            },
            .Block => |block| {
                try stdout.print("Block Statement:\n", .{});
                for (block.statements.items) |block_stmt| {
                    try printStmt(block_stmt, indent + 2);
                }
            },
            .If => |if_stmt| {
                try stdout.print("If Statement:\n", .{});
                try printIndent(indent + 2);
                try stdout.print("Condition:\n", .{});
                try printExpr(if_stmt.condition, indent + 4);
                try printIndent(indent + 2);
                try stdout.print("Then:\n", .{});
                try printStmt(if_stmt.thenBranch, indent + 4);
                if (if_stmt.elseBranch) |else_branch| {
                    try printIndent(indent + 2);
                    try stdout.print("Else:\n", .{});
                    try printStmt(else_branch, indent + 4);
                }
            },
            .While => |while_stmt| {
                try stdout.print("While Statement:\n", .{});
                try printIndent(indent + 2);
                try stdout.print("Condition:\n", .{});
                try printExpr(while_stmt.condition, indent + 4);
                try printIndent(indent + 2);
                try stdout.print("Body:\n", .{});
                try printStmt(while_stmt.body, indent + 4);
            },
            .Function => |func| {
                try stdout.print("Function Declaration: {s}\n", .{func.name.lexeme});
                try printExpr(func.function, indent + 2);
            },
            .Return => |ret| {
                try stdout.print("Return Statement:\n", .{});
                if (ret.value) |value| {
                    try printExpr(value, indent + 2);
                }
            },
        }
    }

    // Print an expression with indentation
    fn printExpr(expr: *ast.Expr, indent: usize) std.fs.File.WriteError!void {
        try printIndent(indent);
        switch (expr.*) {
            .Literal => |lit| {
                switch (lit.value) {
                    .Number => |n| try stdout.print("Number: {d}\n", .{n}),
                    .String => |s| try stdout.print("String: {s}\n", .{s}),
                    .Bool => |b| try stdout.print("Bool: {}\n", .{b}),
                    .Nil => try stdout.print("Nil\n", .{}),
                }
            },
            .Variable => |var_expr| {
                try stdout.print("Variable: {s}\n", .{var_expr.name.lexeme});
            },
            .Binary => |bin| {
                try stdout.print("Binary: {s}\n", .{bin.operator.lexeme});
                try printExpr(bin.left, indent + 2);
                try printExpr(bin.right, indent + 2);
            },
            .Unary => |un| {
                try stdout.print("Unary: {s}\n", .{un.operator.lexeme});
                try printExpr(un.right, indent + 2);
            },
            .Grouping => |group| {
                try stdout.print("Grouping:\n", .{});
                try printExpr(group.expression, indent + 2);
            },
            .Assign => |assign| {
                try stdout.print("Assignment: {s}\n", .{assign.name.lexeme});
                try printExpr(assign.value, indent + 2);
            },
            .Logical => |log| {
                try stdout.print("Logical: {s}\n", .{log.operator.lexeme});
                try printExpr(log.left, indent + 2);
                try printExpr(log.right, indent + 2);
            },
            .Call => |call| {
                try stdout.print("Call:\n", .{});
                try printIndent(indent + 2);
                try stdout.print("Callee:\n", .{});
                try printExpr(call.callee, indent + 4);
                try printIndent(indent + 2);
                try stdout.print("Arguments:\n", .{});
                for (call.arguments.items) |arg| {
                    try printExpr(arg, indent + 4);
                }
            },
            .Function => |func| {
                try stdout.print("Function Expression:\n", .{});
                try printIndent(indent + 2);
                try stdout.print("Parameters:\n", .{});
                for (func.params.items) |param| {
                    try printIndent(indent + 4);
                    try stdout.print("{s}\n", .{param.lexeme});
                }
                try printIndent(indent + 2);
                try stdout.print("Body:\n", .{});
                for (func.body.items) |stmt| {
                    try printStmt(stmt, indent + 4);
                }
            },
        }
    }

    // Print indentation spaces
    fn printIndent(indent: usize) std.fs.File.WriteError!void {
        var i: usize = 0;
        while (i < indent) : (i += 1) {
            try stdout.print(" ", .{});
        }
    }
};

// Testing strategy:
// 1. Test basic literal expressions
// 2. Test nested expressions with memory management
// 3. Test statements with optional fields
// 4. Test complex structures with ArrayList management
// 5. Test full program structure with mixed expressions and statements
test "basic literal expression" {
    const allocator = testing.allocator;

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create a number literal
    const number = try allocator.create(ast.Expr);
    number.* = .{ .Literal = .{ .value = .{ .Number = 42.0 } } };

    // Create an expression statement
    const stmt = try allocator.create(ast.Stmt);
    stmt.* = .{ .Expression = .{ .expression = number } };

    try tree.statements.append(stmt);
    try testing.expectEqual(@as(usize, 1), tree.statements.items.len);
}

test "nested binary expression" {
    const allocator = testing.allocator;

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create 1 + 2
    const left = try allocator.create(ast.Expr);
    left.* = .{ .Literal = .{ .value = .{ .Number = 1.0 } } };

    const right = try allocator.create(ast.Expr);
    right.* = .{ .Literal = .{ .value = .{ .Number = 2.0 } } };

    const binary = try allocator.create(ast.Expr);
    binary.* = .{ .Binary = .{
        .left = left,
        .operator = Token{ .type = .Plus, .lexeme = "+", .line = 1 },
        .right = right,
    } };

    const stmt = try allocator.create(ast.Stmt);
    stmt.* = .{ .Expression = .{ .expression = binary } };

    try tree.statements.append(stmt);
}

test "if statement with optional else" {
    const allocator = testing.allocator;

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    const condition = try allocator.create(ast.Expr);
    condition.* = .{ .Literal = .{ .value = .{ .Bool = true } } };

    const then_body = try allocator.create(ast.Stmt);
    then_body.* = .{ .Print = .{
        .expression = try allocator.create(ast.Expr),
    } };
    then_body.Print.expression.* = .{ .Literal = .{ .value = .{ .String = "then" } } };

    const if_stmt = try allocator.create(ast.Stmt);
    if_stmt.* = .{ .If = .{
        .condition = condition,
        .thenBranch = then_body,
        .elseBranch = null,
    } };

    try tree.statements.append(if_stmt);
}

test "function declaration with parameters" {
    const allocator = testing.allocator;

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    var params = std.ArrayList(scanner.Token).init(allocator);
    try params.append(.{ .type = .Identifier, .lexeme = "x", .line = 1 });

    var body = std.ArrayList(*ast.Stmt).init(allocator);
    const return_expr = try allocator.create(ast.Expr);
    return_expr.* = .{ .Variable = .{ .name = .{ .type = .Identifier, .lexeme = "x", .line = 1 } } };

    const return_stmt = try allocator.create(ast.Stmt);
    return_stmt.* = .{ .Return = .{
        .keyword = .{ .type = .Return, .lexeme = "return", .line = 2 },
        .value = return_expr,
    } };
    try body.append(return_stmt);

    const func_expr = try allocator.create(ast.Expr);
    func_expr.* = .{ .Function = .{
        .params = params,
        .body = body,
    } };

    const func_stmt = try allocator.create(ast.Stmt);
    func_stmt.* = .{ .Function = .{
        .name = .{ .type = .Identifier, .lexeme = "identity", .line = 1 },
        .function = func_expr,
    } };

    try tree.statements.append(func_stmt);
}

test "complex expression tree" {
    const allocator = testing.allocator;

    var tree = ast.Ast.init(allocator);
    defer tree.deinit();

    // Create (1 + 2) * 3
    const one = try allocator.create(ast.Expr);
    one.* = .{ .Literal = .{ .value = .{ .Number = 1.0 } } };

    const two = try allocator.create(ast.Expr);
    two.* = .{ .Literal = .{ .value = .{ .Number = 2.0 } } };

    const plus = try allocator.create(ast.Expr);
    plus.* = .{ .Binary = .{
        .left = one,
        .operator = .{ .type = .Plus, .lexeme = "+", .line = 1 },
        .right = two,
    } };

    const group = try allocator.create(ast.Expr);
    group.* = .{ .Grouping = .{ .expression = plus } };

    const three = try allocator.create(ast.Expr);
    three.* = .{ .Literal = .{ .value = .{ .Number = 3.0 } } };

    const mult = try allocator.create(ast.Expr);
    mult.* = .{ .Binary = .{
        .left = group,
        .operator = .{ .type = .Star, .lexeme = "*", .line = 1 },
        .right = three,
    } };

    const stmt = try allocator.create(ast.Stmt);
    stmt.* = .{ .Expression = .{ .expression = mult } };

    try tree.statements.append(stmt);
}
