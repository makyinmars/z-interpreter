const std = @import("std");
const testing = std.testing;
const ast = @import("ast.zig");
const scanner = @import("scanner.zig");
const Token = scanner.Token;

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
