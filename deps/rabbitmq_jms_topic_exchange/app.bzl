load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_jms_topic_exchange.beam", "ebin/sjx_evaluator.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_jms_topic_exchange_beam",
        srcs = ["src/rabbit_jms_topic_exchange.erl"],
        outs = ["ebin/rabbit_jms_topic_exchange.beam"],
        hdrs = ["include/rabbit_jms_topic_exchange.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_sjx_evaluator_beam",
        srcs = ["src/sjx_evaluator.erl"],
        outs = ["ebin/sjx_evaluator.beam"],
        erlc_opts = "//:erlc_opts",
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_jms_topic_exchange.beam", "test/sjx_evaluator.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_jms_topic_exchange_beam",
        testonly = True,
        srcs = ["src/rabbit_jms_topic_exchange.erl"],
        outs = ["test/rabbit_jms_topic_exchange.beam"],
        hdrs = ["include/rabbit_jms_topic_exchange.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_sjx_evaluator_beam",
        testonly = True,
        srcs = ["src/sjx_evaluator.erl"],
        outs = ["test/sjx_evaluator.beam"],
        erlc_opts = "//:test_erlc_opts",
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/rabbit_jms_topic_exchange.hrl", "src/rabbit_jms_topic_exchange.erl", "src/sjx_evaluator.erl"],
    )
