const std = @import("std");

pub const Node = struct {
    type: NodeUnion,
    pub fn toString(self: *const Node, allocator: std.mem.Allocator) []const u8 {
        switch (self.type) {
            .Number => |n| return n.toString(allocator),
            .String => |s| return s.toString(allocator),
            .BinOp => |b| return b.toString(allocator),
            // .Table => |t| return t.toString(allocator),
            // else => @panic("invalid node type"),
        }
    }
};

pub const NodeUnionTag = enum { Number, String, BinOp };
const NodeUnion = union(NodeUnionTag) {
    Number: Number,
    String: String,
    BinOp: BinOp,
    // Table,
};

const Number = struct {
    val: f64,
    fn toString(self: Number, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(
            allocator,
            "Number({d})",
            .{self.val},
        ) catch @panic("failed to allocate memory");
    }
};

const String = struct {
    val: []const u8,
    fn toString(self: String, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(
            allocator,
            "String(\"{s}\")",
            .{self.val},
        ) catch @panic("failed to allocate memory");
    }
};

// const Table = struct {
//     val: []const Node,
// };
//
// const Fn = struct {
//     name: ?[]const u8,
//     params: []const Node,
//     body: []const Node,
// };
//
// const FnCall = struct {
//     args: []const Node,
// };

const BinOp = struct {
    op: BinOpType,
    lhs: u32,
    rhs: u32,
    fn toString(self: BinOp, allocator: std.mem.Allocator) []const u8 {
        // const lhs = self.lhs.toString(allocator);
        // const rhs = self.rhs.toString(allocator);

        const fstring = std.fmt.allocPrint(
            allocator,
            "Binop<{s}>({}, {})",
            .{ self.op.toString(), self.lhs, self.rhs },
        ) catch @panic("failed to allocate memory");

        // allocator.free(lhs);
        // allocator.free(rhs);
        return fstring;
    }
};

pub const BinOpType = enum {
    Add,
    Sub,
    fn toString(self: BinOpType) []const u8 {
        return switch (self) {
            .Add => "Add",
            .Sub => "Sub",
        };
    }
};
