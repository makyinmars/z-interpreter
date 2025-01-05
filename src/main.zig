const std = @import("std");
const scanner = @import("scanner.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const semantic = @import("semantic.zig");

const stdout = std.io.getStdIn().writer();

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Sample input code to scan
    const source =
        \\var x = 42;
        \\if (x > 10) {
        \\    print("Hello, World! From Z - Language :D");
        \\}
    ;

    // Initialize scanner
    var scnr = scanner.Scanner.init(source, allocator);
    defer scnr.deinit();

    // Scan the source code
    try scnr.scan();

    // Print all tokens
    try stdout.print("Tokens:\n", .{});
    for (scnr.tokens.items) |token| {
        try stdout.print("[Line {d}] {s}: {s}\n", .{
            token.line,
            @tagName(token.type),
            token.lexeme,
        });
    }

    // Initialize parser with tokens
    var prsr = parser.Parser.init(scnr.tokens.items, allocator);
    defer prsr.deinit();

    // Parse the tokens into an AST
    const tree = try prsr.parse();

    // Print the AST structure
    try stdout.print("\nAST Structure: \n", .{});
    try tree.printAst(0);

    // Initialize semantic analyzer
    var analyzer = semantic.SemanticAnalyzer.init(allocator);
    defer analyzer.deinit();

    // Perform semantic analysis
    const semantic_errors = try analyzer.analyze(&tree);

    // Print semantic analysis results
    try stdout.print("\nSemantic Analysis Results:\n", .{});
    if (semantic_errors.len == 0) {
        try stdout.print("No semantic errors found.\n", .{});
    } else {
        try stdout.print("Found {d} semantic error(s):\n", .{semantic_errors.len});
        for (semantic_errors) |error_msg| {
            try stdout.print("{s}\n", .{error_msg});
        }
    }
}
