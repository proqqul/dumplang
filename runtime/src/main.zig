const std = @import("std");

const InstTag = enum(u64) { push, add, mul, jump, jump_if, cmp, _ };

const ValTag = enum(u32) { vnum, vbool, _ };

const Value = packed struct {
    tag: ValTag,
    val: u32,
};

const Instruction = packed struct {
    tag: InstTag,
    val: packed union {
        push: Value,
        add: void,
        mul: void,
        jump: u64,
        jump_if: u64,
        cmp: void,
    },
};

const PROG_SIZE = 1000;
const PROG_BYTES = PROG_SIZE * @sizeOf(Instruction);
const STACK_SIZE = 1000;

pub fn main() !void {
    const file = try std.fs.cwd().openFile("out.bin", .{});
    defer file.close();

    var input: [PROG_SIZE]Instruction = undefined;
    try file.seekTo(0);
    const n = try file.readAll(@ptrCast(*[PROG_BYTES]u8, &input));
    const n_inst = n / @sizeOf(Instruction);
    std.debug.print("{any}\n", .{input[0..n_inst]});
    var res = run(input[0..n_inst]);
    std.debug.print("{any}\n", .{res});
}

const RunError = error{ UnknownInstruction, StackUnderflow, StackLeftOver, TypeError };

fn typeCheck(stack: []Value, args: []const ValTag) RunError!void {
    if (stack.len < args.len) return RunError.StackUnderflow;
    for (args, 0..) |tag, i| {
        if (stack[stack.len - i].tag != tag) {
            return RunError.TypeError;
        }
    }
}

fn run(prog: []const Instruction) RunError!Value {
    var stack: [STACK_SIZE]Value = undefined;
    var sptr: usize = 0;
    var iptr: usize = 0;
    while (iptr < prog.len) : (iptr += 1) {
        const inst = prog[iptr];
        switch (inst.tag) {
            .push => {
                stack[sptr] = inst.val.push;
                sptr += 1;
            },
            .add => {
                try typeCheck(stack[0..sptr], &[_]ValTag{ .vnum, .vnum });
                stack[sptr - 2] = Value{
                    .val = stack[sptr - 2].val + stack[sptr - 1].val,
                    .tag = .vnum,
                };
                sptr -= 1; // pop 2 push 1
            },
            .mul => {
                try typeCheck(stack[0..sptr], &[_]ValTag{ .vnum, .vnum });
                stack[sptr - 2] = Value{
                    .val = stack[sptr - 2].val * stack[sptr - 1].val,
                    .tag = .vnum,
                };
                sptr -= 1; // pop 2 push 1
            },
            .jump => {
                iptr += inst.val.jump;
            },
            .jump_if => {
                sptr -= 1;
                if (stack[sptr].tag != .vbool) return RunError.TypeError;
                if (stack[sptr].val != 0) iptr += inst.val.jump_if;
            },
            .cmp => {
                try typeCheck(stack[0..sptr], &[_]ValTag{ .vnum, .vnum });
                stack[sptr - 2] = Value{
                    // order matters <.<
                    .val = @boolToInt(std.meta.eql(stack[sptr - 2], stack[sptr - 1])),
                    .tag = .vbool,
                };
                sptr -= 1; // pop 2 push 1
            },
            _ => {
                std.debug.print("unknown instruction: {}\n", .{inst.tag});
                return RunError.UnknownInstruction;
            },
        }
    }
    if (sptr == 0) return RunError.StackUnderflow;
    if (sptr > 1) return RunError.StackLeftOver;
    return stack[0];
}
