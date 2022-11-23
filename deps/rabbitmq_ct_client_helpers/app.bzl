load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_ct_client_helpers.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_ct_client_helpers_beam",
        srcs = ["src/rabbit_ct_client_helpers.erl"],
        outs = ["ebin/rabbit_ct_client_helpers.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/rabbit_ct_client_helpers.erl"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_ct_client_helpers.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_ct_client_helpers_beam",
        testonly = True,
        srcs = ["src/rabbit_ct_client_helpers.erl"],
        outs = ["test/rabbit_ct_client_helpers.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
