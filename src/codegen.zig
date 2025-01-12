const std = @import("std");
const ast = @import("ast.zig");
const ir = @import("ir.zig");
const testing = std.testing;
const Allocator = std.mem.Allocator;

/// CodeGen is responsible for generating intermediate representation (IR) code
/// from an abstract syntax tree (AST). It uses an allocator for memory management
/// and maintains a temporary counter for register allocation.
pub const CodeGen = struct {
    ir_module: ir.IR,
    allocator: Allocator,
    temp_counter: u32,

    /// Initializes a new CodeGen instance.
    ///
    /// Parameters:
    /// - allocator: The allocator to use for memory management.
    ///
    /// Returns:
    /// - A new CodeGen instance or an error if initialization fails.
    pub fn init(allocator: Allocator) !CodeGen {
        return .{
            .ir_module = try ir.IR.init(allocator),
            .allocator = allocator,
            .temp_counter = 0,
        };
    }

    /// Deinitializes the CodeGen instance, freeing any allocated resources.
    pub fn deinit(self: *CodeGen) void {
        self.ir_module.deinit();
    }

    /// Generates IR code from the provided AST.
    ///
    /// Parameters:
    /// - tree: The AST to generate code from.
    ///
    /// Returns:
    /// - void or an error if code generation fails.
    pub fn generate(self: *CodeGen, tree: *const ast.Ast) !void {
        for (tree.statements.items) |stmt| {
            try self.generateStmt(stmt);
        }
    }

    /// Generates IR code for a single statement.
    ///
    /// Parameters:
    /// - stmt: The statement to generate code for.
    ///
    /// Returns:
    /// - void or an error if code generation fails.
    fn generateStmt(self: *CodeGen, stmt: *const ast.Stmt) !void {
        switch (stmt.*) {
            .Expression => |expr_stmt| {
                _ = try self.generateExpr(expr_stmt.expression);
            },
            .Print => |print_stmt| {
                const value_reg = try self.generateExpr(print_stmt.expression);
                try self.ir_module.addInstruction(.store, value_reg, null, null);
            },
            .Var => |var_stmt| {
                if (var_stmt.initializer) |initializer| {
                    const value_reg = try self.generateExpr(initializer);
                    try self.ir_module.addInstruction(.store, value_reg, null, null);
                }
            },
            .Block => |block| {
                for (block.statements.items) |block_stmt| {
                    try self.generateStmt(block_stmt);
                }
            },
            .If => |if_stmt| {
                const cond_reg = try self.generateExpr(if_stmt.condition);
                const branch_inst = self.ir_module.instructions.items.len;
                try self.ir_module.addInstruction(.branch, cond_reg, null, null);

                try self.generateStmt(if_stmt.thenBranch);

                if (if_stmt.elseBranch) |else_branch| {
                    const jump_inst = self.ir_module.instructions.items.len;
                    try self.ir_module.addInstruction(.jump, null, null, null);

                    const else_start = self.ir_module.instructions.items.len;
                    try self.generateStmt(else_branch);

                    // Update the branch instruction with the else branch target
                    self.ir_module.instructions.items[branch_inst].operand1 = @intCast(else_start);

                    // Update the jump instruction with the end target
                    const after_else = self.ir_module.instructions.items.len;
                    self.ir_module.instructions.items[jump_inst].operand1 = @intCast(after_else);
                }
            },
            .While => |while_stmt| {
                const loop_start = self.ir_module.instructions.items.len;
                const cond_reg = try self.generateExpr(while_stmt.condition);

                const branch_inst = self.ir_module.instructions.items.len;
                try self.ir_module.addInstruction(.branch, cond_reg, null, null);

                try self.generateStmt(while_stmt.body);

                // Jump back to start
                try self.ir_module.addInstruction(.jump, @intCast(loop_start), null, null);

                // Update branch target to after loop
                const after_loop = self.ir_module.instructions.items.len;
                self.ir_module.instructions.items[branch_inst].operand1 = @intCast(after_loop);
            },
            .Function => |_| {
                // Store the function name/reference
                const func_reg = self.temp_counter;
                self.temp_counter += 1;
                try self.ir_module.addInstruction(.load, func_reg, null, null);
                try self.ir_module.addInstruction(.ret, null, null, null);
            },
            .Return => |ret| {
                if (ret.value) |value| {
                    const reg = try self.generateExpr(value);
                    try self.ir_module.addInstruction(.ret, reg, null, null);
                } else {
                    try self.ir_module.addInstruction(.ret, null, null, null);
                }
            },
        }
    }

    /// Generates IR code for a single expression.
    ///
    /// Parameters:
    /// - expr: The expression to generate code for.
    ///
    /// Returns:
    /// - The register containing the result of the expression or an error if code generation fails.
    fn generateExpr(self: *CodeGen, expr: *const ast.Expr) !u32 {
        var result: u32 = undefined;

        switch (expr.*) {
            .Literal => |_| {
                result = self.temp_counter;
                self.temp_counter += 1;
                try self.ir_module.addInstruction(.load, result, null, null);
            },
            .Variable => |_| {
                try self.ir_module.addInstruction(.load, result, null, null);
            },
            .Binary => |bin| {
                const left_reg = try self.generateExpr(bin.left);
                const right_reg = try self.generateExpr(bin.right);

                result = self.temp_counter;
                self.temp_counter += 1;

                const op: ir.IR.OpCode = switch (bin.operator.type) {
                    .Plus => .add,
                    .Minus => .sub,
                    .Star => .mul,
                    .Slash => .div,
                    else => unreachable,
                };

                try self.ir_module.addInstruction(op, result, left_reg, right_reg);
            },
            .Unary => |un| {
                const operand_reg = try self.generateExpr(un.right);
                if (un.operator.type == .Minus) {
                    const zero = self.temp_counter;
                    self.temp_counter += 1;
                    try self.ir_module.addInstruction(.load, zero, null, null);
                    try self.ir_module.addInstruction(.sub, result, zero, operand_reg);
                }
            },
            .Assign => |assign| {
                const value_reg = try self.generateExpr(assign.value);
                try self.ir_module.addInstruction(.store, value_reg, null, null);
                return value_reg;
            },
            .Logical => |log| {
                const left_reg = try self.generateExpr(log.left);
                const right_reg = try self.generateExpr(log.right);
                try self.ir_module.addInstruction(.add, result, left_reg, right_reg);
            },
            .Call => |call| {
                const callee_reg = try self.generateExpr(call.callee);
                var arg_regs = std.ArrayList(u32).init(self.allocator);
                defer arg_regs.deinit();

                for (call.arguments.items) |arg| {
                    const arg_reg = try self.generateExpr(arg);
                    try arg_regs.append(arg_reg);
                }

                try self.ir_module.addInstruction(.call, result, callee_reg, null);
            },
            .Function => |_| {
                try self.ir_module.addInstruction(.load, result, null, null);
            },
            .Grouping => |group| {
                return try self.generateExpr(group.expression);
            },
        }

        return result;
    }
};

// ### Explanation:
// 1. **Initialization and Deinitialization Test**: This test checks that the `CodeGen` struct is initialized correctly and that the `deinit` method works as expected.
// 2. **Expression Statement Test**: This test verifies that the `generateStmt` method correctly handles an expression statement by generating the appropriate IR instructions.
// 3. **Print Statement Test**: This test checks that the `generateStmt` method correctly handles a print statement by generating the appropriate IR instructions.
// 4. **If Statement Test**: This test verifies that the `generateStmt` method correctly handles an if statement by generating the appropriate IR instructions, including branching.
// 5. **Binary Expression Test**: This test checks that the `generateExpr` method correctly handles a binary expression by generating the appropriate IR instructions.

test "CodeGen initialization and deinitialization" {
    const allocator = testing.allocator;
    var codegen = try CodeGen.init(allocator);
    defer codegen.deinit();

    // Verify that the CodeGen instance is initialized correctly
    try testing.expectEqual(@as(u32, 0), codegen.temp_counter);
    try testing.expect(codegen.ir_module.instructions.items.len == 0);
}

test "CodeGen generateStmt with Expression statement" {
    const allocator = testing.allocator;
    var codegen = try CodeGen.init(allocator);
    defer codegen.deinit();

    const literal = try allocator.create(ast.Expr);
    defer allocator.destroy(literal);
    literal.* = ast.Expr{ .Literal = .{ .value = ast.Value{ .Number = 42 } } };

    const expr_stmt = try allocator.create(ast.Stmt);
    defer allocator.destroy(expr_stmt);
    expr_stmt.* = ast.Stmt{ .Expression = .{ .expression = literal } };

    var tree = ast.Ast{ .statements = std.ArrayList(*ast.Stmt).init(allocator), .allocator = allocator };
    defer tree.statements.deinit();

    try tree.statements.append(expr_stmt);

    // Generate code for the statement
    try codegen.generate(&tree);

    // Verify that the IR contains the expected instructions
    try std.testing.expect(codegen.ir_module.instructions.items.len > 0);
    try std.testing.expectEqual(ir.IR.OpCode.load, codegen.ir_module.instructions.items[0].op);
}

test "CodeGen generateStmt with Print statement" {
    const allocator = std.testing.allocator;
    var codegen = try CodeGen.init(allocator);
    defer codegen.deinit();

    const literal = try allocator.create(ast.Expr);
    defer allocator.destroy(literal);
    literal.* = ast.Expr{ .Literal = .{ .value = ast.Value{ .Number = 42 } } };

    const print_stmt = try allocator.create(ast.Stmt);
    defer allocator.destroy(print_stmt);
    print_stmt.* = ast.Stmt{ .Print = .{ .expression = literal } };

    var tree = ast.Ast{ .statements = std.ArrayList(*ast.Stmt).init(allocator), .allocator = allocator };
    defer tree.statements.deinit();

    try tree.statements.append(print_stmt);

    // Generate code for the statement
    try codegen.generate(&tree);

    // Verify that the IR contains the expected instructions
    try std.testing.expect(codegen.ir_module.instructions.items.len > 0);
    try std.testing.expectEqual(ir.IR.OpCode.load, codegen.ir_module.instructions.items[0].op);
    try std.testing.expectEqual(ir.IR.OpCode.store, codegen.ir_module.instructions.items[1].op);
}

test "CodeGen generateStmt with If statement" {
    const allocator = std.testing.allocator;
    var codegen = try CodeGen.init(allocator);
    defer codegen.deinit();

    const cond_literal = try allocator.create(ast.Expr);
    defer allocator.destroy(cond_literal);
    cond_literal.* = ast.Expr{ .Literal = .{ .value = ast.Value{ .Number = 1 } } };

    const then_literal = try allocator.create(ast.Expr);
    defer allocator.destroy(then_literal);
    then_literal.* = ast.Expr{ .Literal = .{ .value = ast.Value{ .Number = 42 } } };

    const then_expr = try allocator.create(ast.Stmt);
    defer allocator.destroy(then_expr);
    then_expr.* = ast.Stmt{ .Expression = .{ .expression = then_literal } };

    const if_stmt = try allocator.create(ast.Stmt);
    defer allocator.destroy(if_stmt);
    if_stmt.* = ast.Stmt{ .If = .{
        .condition = cond_literal,
        .thenBranch = then_expr,
        .elseBranch = null,
    } };
    var tree = ast.Ast{ .statements = std.ArrayList(*ast.Stmt).init(allocator), .allocator = allocator };
    defer tree.statements.deinit();

    try tree.statements.append(if_stmt);

    // Generate code for the statement
    try codegen.generate(&tree);

    // Verify that the IR contains the expected instructions
    try std.testing.expect(codegen.ir_module.instructions.items.len > 0);
    try std.testing.expectEqual(ir.IR.OpCode.load, codegen.ir_module.instructions.items[0].op);
    try std.testing.expectEqual(ir.IR.OpCode.branch, codegen.ir_module.instructions.items[1].op);
}

test "CodeGen generateExpr with Binary expression" {
    const allocator = std.testing.allocator;
    var codegen = try CodeGen.init(allocator);
    defer codegen.deinit();

    const left_literal = try allocator.create(ast.Expr);
    defer allocator.destroy(left_literal);
    left_literal.* = ast.Expr{ .Literal = .{ .value = ast.Value{ .Number = 10 } } };

    const right_literal = try allocator.create(ast.Expr);
    defer allocator.destroy(right_literal);
    right_literal.* = ast.Expr{ .Literal = .{ .value = ast.Value{ .Number = 20 } } };

    const bin_expr = try allocator.create(ast.Expr);
    defer allocator.destroy(bin_expr);
    bin_expr.* = ast.Expr{ .Binary = .{
        .left = left_literal,
        .right = right_literal,
        .operator = .{ .type = .Plus, .lexeme = "+", .line = 1 },
    } };

    // Generate code for the expression
    // This will:
    // 1. Load left operand into register 0
    // 2. Load right operand into register 1
    // 3. Add them and store result in register 2
    const result_reg = try codegen.generateExpr(bin_expr);

    // Verify that the IR contains the expected instructions
    try testing.expect(codegen.ir_module.instructions.items.len > 0);
    try testing.expectEqual(ir.IR.OpCode.load, codegen.ir_module.instructions.items[0].op);
    try testing.expectEqual(ir.IR.OpCode.load, codegen.ir_module.instructions.items[1].op);
    try testing.expectEqual(ir.IR.OpCode.add, codegen.ir_module.instructions.items[2].op);
    // For binary expression:
    // Register 0: left literal
    // Register 1: right literal
    // Register 2: result of addition
    try testing.expectEqual(@as(u32, 2), result_reg);
    try testing.expectEqual(@as(usize, 3), codegen.ir_module.instructions.items.len);
}
