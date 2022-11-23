load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_exchange_type_random.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_exchange_type_random_beam",
        srcs = ["src/rabbit_exchange_type_random.erl"],
        outs = ["ebin/rabbit_exchange_type_random.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/rabbit_exchange_type_random.erl"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_exchange_type_random.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_exchange_type_random_beam",
        testonly = True,
        srcs = ["src/rabbit_exchange_type_random.erl"],
        outs = ["test/rabbit_exchange_type_random.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
