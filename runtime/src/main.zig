const std = @import("std");

const Instruction = struct {
    tag: u64,
    payload: u64
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

fn run(prog: []Instruction) u64 {
    var stack: [STACK_SIZE]u64 = undefined;
    var stack_ptr: [*]u64 = &stack;
    for (prog) |inst| {
        switch (inst.tag) {
            0 => {
                stack_ptr[0] = inst.payload;
                stack_ptr += 1;
            },
            1 => { 
                stack_ptr -= 1;
                var x = stack_ptr[0];
                stack_ptr -= 1;
                var y = stack_ptr[0];
                stack_ptr[0] = x + y;
                stack_ptr += 1;
            },
            2 => { 
                stack_ptr -= 1;
                var x = stack_ptr[0];
                stack_ptr -= 1;
                var y = stack_ptr[0];
                stack_ptr[0]= x * y;
                stack_ptr += 1;
            },
            else => { std.debug.print("error unimplemented opcode: {any}\n", .{inst.tag}); }
        }
    }
    return stack[0];
}
