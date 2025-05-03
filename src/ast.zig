const std = @import("std");

pub const Node = union(enum) {
    number: Number,
    string: String,
    binop: BinOp,
    boolean: Boolean,
    table: Table,
    pub fn toString(self: Node, allocator: std.mem.Allocator) ![]const u8 {
        switch (self) {
            .number => |n| return n.toString(allocator),
            .string => |s| return s.toString(allocator),
            .binop => |b| return b.toString(allocator),
            .boolean => |b| return b.toString(allocator),
            .table => |t| return t.toString(allocator),
        }
    }
};

const Boolean = struct {
    val: bool,
    fn toString(self: Boolean, allocator: std.mem.Allocator) ![]const u8 {
        return allocator.dupe(u8, if (self.val) "True" else "False");
    }
};

const Number = struct {
    val: f64,
    fn toString(self: Number, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(
            allocator,
            "Number({d})",
            .{self.val},
        );
    }
};

const String = struct {
    val: []const u8,
    fn toString(self: String, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(
            allocator,
            "String(\"{s}\")",
            .{self.val},
        );
    }
};

const Table = struct {
    items: []const u32,
    fn toString(self: Table, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(
            allocator,
            "Table({any})",
            .{self.items},
        );
    }
};

// const Fn = struct {
//     // name: ?[]const u8,
//     // params: []const Node,
//     body: []const Node,
// };

const BinOp = struct {
    op: BinOpType,
    lhs: u32,
    rhs: u32,
    fn toString(self: BinOp, allocator: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(
            allocator,
            "Binop<{s}>({}, {})",
            .{ self.op.toString(), self.lhs, self.rhs },
        );
    }
};

pub const BinOpType = enum {
    Add,
    Sub,
    Multiply,
    fn toString(self: BinOpType) []const u8 {
        return switch (self) {
            .Add => "Add",
            .Sub => "Sub",
            .Multiply => "Multiply",
        };
    }
};
