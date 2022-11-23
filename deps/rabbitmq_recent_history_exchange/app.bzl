load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_exchange_type_recent_history.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_exchange_type_recent_history_beam",
        srcs = ["src/rabbit_exchange_type_recent_history.erl"],
        outs = ["ebin/rabbit_exchange_type_recent_history.beam"],
        hdrs = ["include/rabbit_recent_history.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_exchange_type_recent_history.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_exchange_type_recent_history_beam",
        testonly = True,
        srcs = ["src/rabbit_exchange_type_recent_history.erl"],
        outs = ["test/rabbit_exchange_type_recent_history.beam"],
        hdrs = ["include/rabbit_recent_history.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/rabbit_recent_history.hrl", "src/rabbit_exchange_type_recent_history.erl"],
    )
