const std = @import("std");
const scanner = @import("scanner/scanner.zig");

const stdout = std.io.getStdIn().writer();

pub fn main() !void {
    const allocator = std.heap.page_allocator;

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
}
