load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_shovel_mgmt.beam", "ebin/rabbit_shovel_mgmt_util.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_shovel_mgmt_beam",
        srcs = ["src/rabbit_shovel_mgmt.erl"],
        outs = ["ebin/rabbit_shovel_mgmt.beam"],
        hdrs = ["src/rabbit_shovel_mgmt.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_shovel_mgmt_util_beam",
        srcs = ["src/rabbit_shovel_mgmt_util.erl"],
        outs = ["ebin/rabbit_shovel_mgmt_util.beam"],
        hdrs = ["src/rabbit_shovel_mgmt.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_shovel_mgmt.beam", "test/rabbit_shovel_mgmt_util.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_shovel_mgmt_beam",
        testonly = True,
        srcs = ["src/rabbit_shovel_mgmt.erl"],
        outs = ["test/rabbit_shovel_mgmt.beam"],
        hdrs = ["src/rabbit_shovel_mgmt.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_shovel_mgmt_util_beam",
        testonly = True,
        srcs = ["src/rabbit_shovel_mgmt_util.erl"],
        outs = ["test/rabbit_shovel_mgmt_util.beam"],
        hdrs = ["src/rabbit_shovel_mgmt.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/rabbit_shovel_mgmt.erl", "src/rabbit_shovel_mgmt.hrl", "src/rabbit_shovel_mgmt_util.erl"],
    )
