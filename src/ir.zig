const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;

/// Represents the Intermediate Representation (IR) of the compiled code
/// This IR serves as a bridge between the high-level source code and low-level target code
pub const IR = struct {
    /// Type of operation in IR instruction
    pub const OpCode = enum {
        add, // Addition
        sub, // Subtraction
        mul, // Multiplication
        div, // Division
        load, // Load from memory
        store, // Store to memory
        branch, // Conditional branch
        jump, // Unconditional jump
        ret, // Return
    };

    /// Single IR instruction containing operation and operands
    pub const Instruction = struct {
        op: OpCode,
        dest: ?u32, // Destination register/variable
        operand1: ?u32, // First operand
        operand2: ?u32, // Second operand
    };

    allocator: Allocator,
    instructions: std.ArrayList(Instruction),

    /// Initialize a new IR instance
    pub fn init(allocator: Allocator) !IR {
        return IR{
            .allocator = allocator,
            .instructions = std.ArrayList(Instruction).init(allocator),
        };
    }

    /// Clean up IR resources
    pub fn deinit(self: *IR) void {
        self.instructions.deinit();
    }

    /// Add a new instruction to the IR
    pub fn addInstruction(self: *IR, op: OpCode, dest: ?u32, op1: ?u32, op2: ?u32) !void {
        try self.instructions.append(.{
            .op = op,
            .dest = dest,
            .operand1 = op1,
            .operand2 = op2,
        });
    }

    /// Get the number of instructions in the IR
    pub fn getInstructionCount(self: *const IR) usize {
        return self.instructions.items.len;
    }

    /// Get instruction at specific index
    pub fn getInstruction(self: *const IR, index: usize) ?Instruction {
        if (index >= self.instructions.items.len) return null;
        return self.instructions.items[index];
    }

    /// Print IR for debugging purposes
    pub fn print(self: *const IR, writer: anytype) !void {
        for (self.instructions.items, 0..) |inst, i| {
            try writer.print("Instruction {d}: {s}", .{ i, @tagName(inst.op) });
            if (inst.dest) |d| try writer.print(" dest:{d}", .{d});
            if (inst.operand1) |op1| try writer.print(" op1:{d}", .{op1});
            if (inst.operand2) |op2| try writer.print(" op2:{d}", .{op2});
            try writer.print("\n", .{});
        }
    }
};

// The suite includes tests for:
// - Basic initialization and cleanup
// - Instruction addition and retrieval
// - Null operand handling
// - Invalid access checks
// - Multiple instruction management
// - Print functionality verification
// - Complete OpCode coverage

test "IR - initialization and deinitialization" {
    const allocator = testing.allocator;
    var ir = try IR.init(allocator);
    defer ir.deinit();

    try testing.expect(ir.getInstructionCount() == 0);
}

test "IR - basic instruction operations" {
    const allocator = testing.allocator;
    var ir = try IR.init(allocator);
    defer ir.deinit();

    // Test adding instruction with all fields
    try ir.addInstruction(.add, 1, 2, 3);
    try testing.expect(ir.getInstructionCount() == 1);

    // Test instruction retrieval
    const inst = ir.getInstruction(0).?;
    try testing.expect(inst.op == .add);
    try testing.expect(inst.dest.? == 1);
    try testing.expect(inst.operand1.? == 2);
    try testing.expect(inst.operand2.? == 3);
}

test "IR - null operands" {
    const allocator = testing.allocator;
    var ir = try IR.init(allocator);
    defer ir.deinit();

    try ir.addInstruction(.ret, null, null, null);
    const inst = ir.getInstruction(0).?;
    try testing.expect(inst.op == .ret);
    try testing.expect(inst.dest == null);
    try testing.expect(inst.operand1 == null);
    try testing.expect(inst.operand2 == null);
}

test "IR - invalid instruction access" {
    const allocator = testing.allocator;
    var ir = try IR.init(allocator);
    defer ir.deinit();

    try testing.expect(ir.getInstruction(0) == null);
    try testing.expect(ir.getInstruction(999) == null);
}

test "IR - multiple instructions" {
    const allocator = testing.allocator;
    var ir = try IR.init(allocator);
    defer ir.deinit();

    try ir.addInstruction(.load, 1, 2, null);
    try ir.addInstruction(.store, 3, 4, null);
    try ir.addInstruction(.add, 5, 6, 7);

    try testing.expect(ir.getInstructionCount() == 3);
    try testing.expect(ir.getInstruction(1).?.op == .store);
}

test "IR - print output" {
    const allocator = testing.allocator;
    var ir = try IR.init(allocator);
    defer ir.deinit();

    try ir.addInstruction(.add, 1, 2, 3);

    var list = std.ArrayList(u8).init(allocator);
    defer list.deinit();

    try ir.print(list.writer());

    const output = list.items;
    try testing.expect(std.mem.indexOf(u8, output, "add") != null);
    try testing.expect(std.mem.indexOf(u8, output, "dest:1") != null);
    try testing.expect(std.mem.indexOf(u8, output, "op1:2") != null);
    try testing.expect(std.mem.indexOf(u8, output, "op2:3") != null);
}

test "IR - all opcodes" {
    const allocator = testing.allocator;
    var ir = try IR.init(allocator);
    defer ir.deinit();

    inline for (std.meta.fields(IR.OpCode)) |field| {
        try ir.addInstruction(@field(IR.OpCode, field.name), 1, 2, 3);
        const inst = ir.getInstruction(ir.getInstructionCount() - 1).?;
        try testing.expect(inst.op == @field(IR.OpCode, field.name));
    }
}
