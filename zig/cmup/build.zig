const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "zmup",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    exe.addIncludePath(b.path("include"));
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    const install_step = b.getInstallStep();

    run_cmd.step.dependOn(install_step);

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const pages_path = std.fs.path.relative(b.allocator, b.install_path, "/usr/share/man/man1/zmup.1") catch return;

    const file = b.addInstallFile(b.path("./man.1"), pages_path);
    install_step.dependOn(&file.step);

    const run_step = b.step("run", "Run the cmup");
    run_step.dependOn(&run_cmd.step);
}
