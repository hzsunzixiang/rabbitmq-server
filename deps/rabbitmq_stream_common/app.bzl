load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_stream_core.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_stream_core_beam",
        srcs = ["src/rabbit_stream_core.erl"],
        outs = ["ebin/rabbit_stream_core.beam"],
        hdrs = ["include/rabbit_stream.hrl"],
        erlc_opts = "//:erlc_opts",
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_stream_core.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_stream_core_beam",
        testonly = True,
        srcs = ["src/rabbit_stream_core.erl"],
        outs = ["test/rabbit_stream_core.beam"],
        hdrs = ["include/rabbit_stream.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/rabbit_stream.hrl", "src/rabbit_stream_core.erl"],
    )
