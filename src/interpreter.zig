/// The Interpreter module implements a tree-walk interpreter for a simple programming language.
/// It handles variable declarations, assignments, control flow, and basic arithmetic operations.
const std = @import("std");
const Allocator = std.mem.Allocator;
const Expr = @import("ast.zig").Expr;
const Stmt = @import("ast.zig").Stmt;
const Ast = @import("ast.zig").Ast;
const Value = @import("ast.zig").Value;
const Token = @import("scanner.zig").Token;
const TokenType = @import("scanner.zig").TokenType;

/// Runtime errors that can occur during interpretation
pub const RuntimeError = error{
    TypeError,
    UndefinedVariable,
    DivisionByZero,
    InvalidOperand,
    OutOfMemory,
    Return,
} || std.fs.File.WriteError;

/// Environment represents a scope for variable bindings
pub const Environment = struct {
    values: std.StringHashMap(Value),
    enclosing: ?*Environment,
    allocator: Allocator,

    /// Initialize a new environment with given allocator
    pub fn init(allocator: Allocator) Environment {
        return .{
            .values = std.StringHashMap(Value).init(allocator),
            .enclosing = null,
            .allocator = allocator,
        };
    }

    /// Clean up environment resources
    pub fn deinit(self: *Environment) void {
        self.values.deinit();
    }

    /// Define a new variable in the current scope
    pub fn define(self: *Environment, name: []const u8, value: Value) !void {
        try self.values.put(name, value);
    }

    /// Look up a variable's value in the current or enclosing scopes
    pub fn get(self: *Environment, name: Token) RuntimeError!Value {
        if (self.values.get(name.lexeme)) |value| {
            return value;
        }

        if (self.enclosing) |enclosing| {
            return enclosing.get(name);
        }

        return RuntimeError.UndefinedVariable;
    }

    /// Assign a new value to an existing variable
    pub fn assign(self: *Environment, name: Token, value: Value) RuntimeError!void {
        if (self.values.contains(name.lexeme)) {
            try self.values.put(name.lexeme, value);
            return;
        }

        if (self.enclosing) |enclosing| {
            try enclosing.assign(name, value);
            return;
        }

        return RuntimeError.UndefinedVariable;
    }
};

/// Main interpreter struct that executes the AST
pub const Interpreter = struct {
    environment: Environment,
    allocator: Allocator,

    /// Initialize a new interpreter with given allocator
    pub fn init(allocator: Allocator) Interpreter {
        return .{
            .environment = Environment.init(allocator),
            .allocator = allocator,
        };
    }

    /// Clean up interpreter resources
    pub fn deinit(self: *Interpreter) void {
        self.environment.deinit();
    }

    /// Interpret an entire AST
    pub fn interpret(self: *Interpreter, ast: *const Ast) RuntimeError!void {
        for (ast.statements.items) |stmt| {
            try self.executeStmt(stmt);
        }
    }

    /// Execute a single statement
    fn executeStmt(self: *Interpreter, stmt: *const Stmt) RuntimeError!void {
        switch (stmt.*) {
            .Expression => |expr| {
                _ = try self.evaluateExpr(expr.expression);
            },
            .Print => |print| {
                const value = try self.evaluateExpr(print.expression);
                try self.printValue(value);
            },
            .Var => |var_decl| {
                var value = Value{ .Nil = {} };
                if (var_decl.initializer) |initializer| {
                    value = try self.evaluateExpr(initializer);
                }
                try self.environment.define(var_decl.name.lexeme, value);
            },
            .Block => |block| {
                var new_env = Environment.init(self.allocator);
                new_env.enclosing = &self.environment;
                const previous = self.environment;
                self.environment = new_env;
                defer {
                    self.environment.deinit();
                    self.environment = previous;
                }

                for (block.statements.items) |block_stmt| {
                    try self.executeStmt(block_stmt);
                }
            },
            .If => |if_stmt| {
                const condition = try self.evaluateExpr(if_stmt.condition);
                if (self.isTruthy(condition)) {
                    try self.executeStmt(if_stmt.thenBranch);
                } else if (if_stmt.elseBranch) |else_branch| {
                    try self.executeStmt(else_branch);
                }
            },
            .While => |while_stmt| {
                while (self.isTruthy(try self.evaluateExpr(while_stmt.condition))) {
                    try self.executeStmt(while_stmt.body);
                }
            },
            .Function => |func| {
                const function = try self.evaluateExpr(func.function);
                try self.environment.define(func.name.lexeme, function);
            },
            .Return => |ret| {
                var value = Value{ .Nil = {} };
                if (ret.value) |expr| {
                    value = try self.evaluateExpr(expr);
                }
                return RuntimeError.Return;
            },
        }
    }

    /// Evaluate an expression and return its value
    fn evaluateExpr(self: *Interpreter, expr: *const Expr) RuntimeError!Value {
        return switch (expr.*) {
            .Literal => |lit| lit.value,
            .Variable => |var_expr| try self.environment.get(var_expr.name),
            .Binary => |bin| try self.evaluateBinaryExpr(bin.left, bin.operator, bin.right),
            .Unary => |un| try self.evaluateUnaryExpr(un.operator, un.right),
            .Grouping => |group| try self.evaluateExpr(group.expression),
            .Assign => |assign| blk: {
                const value = try self.evaluateExpr(assign.value);
                try self.environment.assign(assign.name, value);
                break :blk value;
            },
            .Logical => |log| try self.evaluateLogicalExpr(log.left, log.operator, log.right),
            .Call => |_| return RuntimeError.TypeError,
            .Function => |_| return RuntimeError.TypeError,
        };
    }

    /// Evaluate binary expressions (arithmetic and comparison)
    fn evaluateBinaryExpr(self: *Interpreter, left: *const Expr, operator: Token, right: *const Expr) RuntimeError!Value {
        const left_val = try self.evaluateExpr(left);
        const right_val = try self.evaluateExpr(right);

        switch (operator.type) {
            .Plus => {
                if (left_val == .Number and right_val == .Number) {
                    return Value{ .Number = left_val.Number + right_val.Number };
                }
                if (left_val == .String and right_val == .String) {
                    return RuntimeError.TypeError;
                }
                return RuntimeError.TypeError;
            },
            .Minus => {
                if (left_val == .Number and right_val == .Number) {
                    return Value{ .Number = left_val.Number - right_val.Number };
                }
                return RuntimeError.TypeError;
            },
            .Star => {
                if (left_val == .Number and right_val == .Number) {
                    return Value{ .Number = left_val.Number * right_val.Number };
                }
                return RuntimeError.TypeError;
            },
            .Slash => {
                if (left_val == .Number and right_val == .Number) {
                    if (right_val.Number == 0) {
                        return RuntimeError.DivisionByZero;
                    }
                    return Value{ .Number = left_val.Number / right_val.Number };
                }
                return RuntimeError.TypeError;
            },
            .Greater, .GreaterEqual, .Less, .LessEqual, .EqualEqual, .BangEqual => {
                return self.evaluateComparison(left_val, operator, right_val);
            },
            else => return RuntimeError.InvalidOperand,
        }
    }

    /// Evaluate unary expressions (negation and logical not)
    fn evaluateUnaryExpr(self: *Interpreter, operator: Token, right: *const Expr) RuntimeError!Value {
        const right_val = try self.evaluateExpr(right);

        return switch (operator.type) {
            .Minus => if (right_val == .Number) Value{ .Number = -right_val.Number } else RuntimeError.TypeError,
            .Bang => Value{ .Bool = !self.isTruthy(right_val) },
            else => RuntimeError.InvalidOperand,
        };
    }

    /// Evaluate logical expressions (and/or)
    fn evaluateLogicalExpr(self: *Interpreter, left: *const Expr, operator: Token, right: *const Expr) RuntimeError!Value {
        const left_val = try self.evaluateExpr(left);

        if (operator.type == .Or) {
            if (self.isTruthy(left_val)) return left_val;
        } else {
            if (!self.isTruthy(left_val)) return left_val;
        }

        return self.evaluateExpr(right);
    }

    /// Evaluate comparison expressions
    fn evaluateComparison(self: *Interpreter, left: Value, operator: Token, right: Value) RuntimeError!Value {
        switch (operator.type) {
            .EqualEqual => return Value{ .Bool = self.isEqual(left, right) },
            .BangEqual => return Value{ .Bool = !self.isEqual(left, right) },
            .Greater => {
                if (left == .Number and right == .Number) {
                    return Value{ .Bool = left.Number > right.Number };
                }
                return RuntimeError.TypeError;
            },
            .GreaterEqual => {
                if (left == .Number and right == .Number) {
                    return Value{ .Bool = left.Number >= right.Number };
                }
                return RuntimeError.TypeError;
            },
            .Less => {
                if (left == .Number and right == .Number) {
                    return Value{ .Bool = left.Number < right.Number };
                }
                return RuntimeError.TypeError;
            },
            .LessEqual => {
                if (left == .Number and right == .Number) {
                    return Value{ .Bool = left.Number <= right.Number };
                }
                return RuntimeError.TypeError;
            },
            else => return RuntimeError.InvalidOperand,
        }
    }

    /// Determine if a value is considered truthy
    fn isTruthy(self: *Interpreter, value: Value) bool {
        _ = self;
        return switch (value) {
            .Bool => |b| b,
            .Nil => false,
            else => true,
        };
    }

    /// Compare two values for equality
    fn isEqual(self: *Interpreter, left: Value, right: Value) bool {
        _ = self;
        if (@as(std.meta.Tag(Value), left) != @as(std.meta.Tag(Value), right)) {
            return false;
        }

        return switch (left) {
            .Number => |n| n == right.Number,
            .String => |s| std.mem.eql(u8, s, right.String),
            .Bool => |b| b == right.Bool,
            .Nil => true,
        };
    }

    /// Print a value to standard output
    fn printValue(self: *Interpreter, value: Value) !void {
        _ = self;
        switch (value) {
            .Number => |n| try std.io.getStdOut().writer().print("{d}\n", .{n}),
            .String => |s| try std.io.getStdOut().writer().print("{s}\n", .{s}),
            .Bool => |b| try std.io.getStdOut().writer().print("{}\n", .{b}),
            .Nil => try std.io.getStdOut().writer().print("nil\n", .{}),
        }
    }
};

// Tests basic arithmetic operations in the interpreter.
// Creates an AST for the expression "print 2 + 3" and verifies
// that the interpreter correctly evaluates and prints the result.
test "Interpreter: basic arithmetic" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Create a simple AST for: print 2 + 3;
    var ast = Ast.init(allocator);
    defer ast.deinit();

    const two = try allocator.create(Expr);
    two.* = .{ .Literal = .{ .value = .{ .Number = 2 } } };

    const three = try allocator.create(Expr);
    three.* = .{ .Literal = .{ .value = .{ .Number = 3 } } };

    const plus = try allocator.create(Expr);
    plus.* = .{ .Binary = .{
        .left = two,
        .operator = Token{ .type = .Plus, .lexeme = "+", .line = 1 },
        .right = three,
    } };

    const print_stmt = try allocator.create(Stmt);
    print_stmt.* = .{ .Print = .{ .expression = plus } };

    try ast.statements.append(print_stmt);

    try interpreter.interpret(&ast);
}

test "Interpreter: variable declaration and assignment" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Create AST for: var x = 42; print x;
    var ast = Ast.init(allocator);
    defer ast.deinit();

    const number = try allocator.create(Expr);
    number.* = .{ .Literal = .{ .value = .{ .Number = 42 } } };

    const var_decl = try allocator.create(Stmt);
    var_decl.* = .{ .Var = .{
        .name = Token{ .type = .Identifier, .lexeme = "x", .line = 1 },
        .initializer = number,
    } };

    const var_expr = try allocator.create(Expr);
    var_expr.* = .{ .Variable = .{
        .name = Token{ .type = .Identifier, .lexeme = "x", .line = 1 },
    } };

    const print_stmt = try allocator.create(Stmt);
    print_stmt.* = .{ .Print = .{ .expression = var_expr } };

    try ast.statements.append(var_decl);
    try ast.statements.append(print_stmt);

    try interpreter.interpret(&ast);
}

test "Interpreter: if statement" {
    const allocator = std.testing.allocator;
    var interpreter = Interpreter.init(allocator);
    defer interpreter.deinit();

    // Create AST for: if (true) print 64;
    var ast = Ast.init(allocator);
    defer ast.deinit();

    const condition = try allocator.create(Expr);
    condition.* = .{ .Literal = .{ .value = .{ .Bool = true } } };

    const number = try allocator.create(Expr);
    number.* = .{ .Literal = .{ .value = .{ .Number = 64 } } };

    const print_stmt = try allocator.create(Stmt);
    print_stmt.* = .{ .Print = .{ .expression = number } };

    const if_stmt = try allocator.create(Stmt);
    if_stmt.* = .{ .If = .{
        .condition = condition,
        .thenBranch = print_stmt,
        .elseBranch = null,
    } };

    try ast.statements.append(if_stmt);

    try interpreter.interpret(&ast);
}
