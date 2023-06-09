const std = @import("std");

const InstTag = enum(u64) { push, add, mul, jump, jump_if, cmp, copy, trunc_stack, _ };

const ValTag = enum(u32) { vnum, vbool, _ };

const Value = packed struct {
    tag: ValTag,
    val: u32,
};

const Instruction = packed struct {
    tag: InstTag,
    val: packed union {
        push: Value,
        add: packed struct { u32, u32 },
        mul: packed struct { u32, u32 },
        jump: packed struct { jump_by: u32, padding: u32 },
        jump_if: packed struct { jump_by: u32, comp_addr: u32 },
        cmp: packed struct { u32, u32 },
        copy: packed struct { addr: u32, padding: u32 },
        trunc_stack: packed struct { addr: u32, padding: u32 },
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

fn NTuple(comptime N: comptime_int, comptime t: type) type {
    const foo = &[_]type{t} ** N;
    return std.meta.Tuple(foo);
}

fn typeCheck(stack: []Value, comptime types: []const ValTag, args: NTuple(types.len, u32)) RunError!void {
    inline for (types, args) |t, a| {
        if (stack[a].tag != t) {
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
                try typeCheck(&stack, &[_]ValTag{ .vnum, .vnum }, inst.val.add);
                stack[sptr] = Value{
                    .val = stack[inst.val.add.@"0"].val + stack[inst.val.add.@"1"].val,
                    .tag = .vnum,
                };
                sptr += 1;
            },
            .mul => {
                try typeCheck(&stack, &[_]ValTag{ .vnum, .vnum }, inst.val.mul);
                stack[sptr] = Value{
                    .val = stack[inst.val.mul.@"0"].val * stack[inst.val.mul.@"1"].val,
                    .tag = .vnum,
                };
                sptr += 1;
            },
            .jump => {
                iptr += inst.val.jump.jump_by;
            },
            .jump_if => {
                if (stack[inst.val.jump_if.comp_addr].tag != .vbool) return RunError.TypeError;
                if (stack[inst.val.jump_if.comp_addr].val != 0) iptr += inst.val.jump_if.jump_by;
            },
            .cmp => {
                try typeCheck(&stack, &[_]ValTag{ .vnum, .vnum }, inst.val.cmp);
                stack[sptr] = Value{
                    // order matters <.<
                    .val = @boolToInt(std.meta.eql(stack[inst.val.cmp.@"0"], stack[inst.val.cmp.@"1"])),
                    .tag = .vbool,
                };
                sptr += 1;
            },
            .copy => {
                stack[sptr] = stack[inst.val.copy.addr];
                sptr += 1;
            },
            .trunc_stack => {
                sptr = inst.val.trunc_stack.addr;
            },
            _ => {
                std.debug.print("unknown instruction: {}\n", .{inst.tag});
                return RunError.UnknownInstruction;
            },
        }
    }
    if (sptr == 0) return RunError.StackUnderflow;
    if (sptr > 1) return RunError.StackLeftOver;
    // FIXME: returning stack-allocated value (¬‿¬)
    return stack[0];
}
