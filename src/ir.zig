const std = @import("std");

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

    allocator: std.mem.Allocator,
    instructions: std.ArrayList(Instruction),

    /// Initialize a new IR instance
    pub fn init(allocator: std.mem.Allocator) !IR {
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
