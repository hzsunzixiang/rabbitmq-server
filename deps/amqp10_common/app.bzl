load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    erlang_bytecode(
        name = "ebin_amqp10_binary_generator_beam",
        srcs = ["src/amqp10_binary_generator.erl"],
        outs = ["ebin/amqp10_binary_generator.beam"],
        hdrs = ["include/amqp10_framing.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_amqp10_binary_parser_beam",
        srcs = ["src/amqp10_binary_parser.erl"],
        outs = ["ebin/amqp10_binary_parser.beam"],
        hdrs = ["include/amqp10_framing.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_amqp10_framing_beam",
        srcs = ["src/amqp10_framing.erl"],
        outs = ["ebin/amqp10_framing.beam"],
        hdrs = ["include/amqp10_framing.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    filegroup(
        name = "beam_files",
        srcs = ["ebin/amqp10_binary_generator.beam", "ebin/amqp10_binary_parser.beam", "ebin/amqp10_framing.beam", "ebin/amqp10_framing0.beam"],
    )
    erlang_bytecode(
        name = "ebin_amqp10_framing0_beam",
        srcs = ["src/amqp10_framing0.erl"],
        outs = ["ebin/amqp10_framing0.beam"],
        hdrs = ["include/amqp10_framing.hrl"],
        erlc_opts = "//:erlc_opts",
    )

def all_test_beam_files():
    erlang_bytecode(
        name = "test_amqp10_binary_generator_beam",
        testonly = True,
        srcs = ["src/amqp10_binary_generator.erl"],
        outs = ["test/amqp10_binary_generator.beam"],
        hdrs = ["include/amqp10_framing.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_amqp10_binary_parser_beam",
        testonly = True,
        srcs = ["src/amqp10_binary_parser.erl"],
        outs = ["test/amqp10_binary_parser.beam"],
        hdrs = ["include/amqp10_framing.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_amqp10_framing_beam",
        testonly = True,
        srcs = ["src/amqp10_framing.erl"],
        outs = ["test/amqp10_framing.beam"],
        hdrs = ["include/amqp10_framing.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_amqp10_framing0_beam",
        testonly = True,
        srcs = ["src/amqp10_framing0.erl"],
        outs = ["test/amqp10_framing0.beam"],
        hdrs = ["include/amqp10_framing.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/amqp10_binary_generator.beam", "test/amqp10_binary_parser.beam", "test/amqp10_framing.beam", "test/amqp10_framing0.beam"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/amqp10_framing.hrl", "src/amqp10_binary_generator.erl", "src/amqp10_binary_parser.erl", "src/amqp10_framing.erl", "src/amqp10_framing0.erl"],
    )
