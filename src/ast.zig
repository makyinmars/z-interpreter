const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("scanner.zig").Token;
const testing = std.testing;
const scanner = @import("scanner.zig");

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
};
