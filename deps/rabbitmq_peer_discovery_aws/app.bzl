load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_peer_discovery_aws.beam", "ebin/rabbitmq_peer_discovery_aws.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_peer_discovery_aws_beam",
        srcs = ["src/rabbit_peer_discovery_aws.erl"],
        outs = ["ebin/rabbit_peer_discovery_aws.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_peer_discovery_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_peer_discovery_aws_beam",
        srcs = ["src/rabbitmq_peer_discovery_aws.erl"],
        outs = ["ebin/rabbitmq_peer_discovery_aws.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_peer_discovery_aws.beam", "test/rabbitmq_peer_discovery_aws.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_peer_discovery_aws_beam",
        testonly = True,
        srcs = ["src/rabbit_peer_discovery_aws.erl"],
        outs = ["test/rabbit_peer_discovery_aws.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_peer_discovery_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbitmq_peer_discovery_aws_beam",
        testonly = True,
        srcs = ["src/rabbitmq_peer_discovery_aws.erl"],
        outs = ["test/rabbitmq_peer_discovery_aws.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/rabbit_peer_discovery_aws.erl", "src/rabbitmq_peer_discovery_aws.erl"],
    )
