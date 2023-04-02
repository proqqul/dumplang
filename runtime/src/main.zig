const std = @import("std");

const Instruction = packed struct { tag: u64, payload: u64 };

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

const RunError = error{ UnknownInstruction, StackUnderflow, StackLeftOver };

fn run(prog: []const Instruction) RunError!u64 {
    var stack: [STACK_SIZE]u64 = undefined;
    var sptr: usize = 0;
    for (prog) |inst| {
        switch (inst.tag) {
            0 => {
                stack[sptr] = inst.payload;
                sptr += 1;
            },
            1 => {
                if (sptr < 2) return RunError.StackUnderflow;
                stack[sptr - 2] = stack[sptr - 2] + stack[sptr - 1];
                sptr -= 1;
            },
            2 => {
                if (sptr < 2) return RunError.StackUnderflow;
                stack[sptr - 2] = stack[sptr - 2] * stack[sptr - 1];
                sptr -= 1;
            },
            else => {
                std.debug.print("unknown instruction: {}", .{inst.tag});
                return RunError.UnknownInstruction;
            },
        }
    }
    if (sptr == 0) return RunError.StackUnderflow;
    if (sptr > 1) return RunError.StackLeftOver;
    return stack[0];
}
