const std = @import("std");
const scanner = @import("scanner.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

const stdout = std.io.getStdIn().writer();

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Sample input code to scan
    const source =
        \\var x = 42;
        \\if (x > 10) {
        \\    print("Hello, World!");
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
}
