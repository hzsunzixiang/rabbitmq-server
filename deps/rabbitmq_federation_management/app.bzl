load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_federation_mgmt.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_mgmt_beam",
        srcs = ["src/rabbit_federation_mgmt.erl"],
        outs = ["ebin/rabbit_federation_mgmt.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_federation_mgmt.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_mgmt_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_mgmt.erl"],
        outs = ["test/rabbit_federation_mgmt.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/rabbit_federation_mgmt.erl"],
    )
