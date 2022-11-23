load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_peer_discovery_etcd.beam", "ebin/rabbitmq_peer_discovery_etcd.beam", "ebin/rabbitmq_peer_discovery_etcd_app.beam", "ebin/rabbitmq_peer_discovery_etcd_sup.beam", "ebin/rabbitmq_peer_discovery_etcd_v3_client.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_peer_discovery_etcd_beam",
        srcs = ["src/rabbit_peer_discovery_etcd.erl"],
        outs = ["ebin/rabbit_peer_discovery_etcd.beam"],
        hdrs = ["include/rabbit_peer_discovery_etcd.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_peer_discovery_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_peer_discovery_etcd_app_beam",
        srcs = ["src/rabbitmq_peer_discovery_etcd_app.erl"],
        outs = ["ebin/rabbitmq_peer_discovery_etcd_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_peer_discovery_etcd_beam",
        srcs = ["src/rabbitmq_peer_discovery_etcd.erl"],
        outs = ["ebin/rabbitmq_peer_discovery_etcd.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_peer_discovery_etcd_sup_beam",
        srcs = ["src/rabbitmq_peer_discovery_etcd_sup.erl"],
        outs = ["ebin/rabbitmq_peer_discovery_etcd_sup.beam"],
        hdrs = ["include/rabbit_peer_discovery_etcd.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_peer_discovery_etcd_v3_client_beam",
        srcs = ["src/rabbitmq_peer_discovery_etcd_v3_client.erl"],
        outs = ["ebin/rabbitmq_peer_discovery_etcd_v3_client.beam"],
        hdrs = ["include/rabbit_peer_discovery_etcd.hrl"],
        erlc_opts = "//:erlc_opts",
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_peer_discovery_etcd.beam", "test/rabbitmq_peer_discovery_etcd.beam", "test/rabbitmq_peer_discovery_etcd_app.beam", "test/rabbitmq_peer_discovery_etcd_sup.beam", "test/rabbitmq_peer_discovery_etcd_v3_client.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_peer_discovery_etcd_beam",
        testonly = True,
        srcs = ["src/rabbit_peer_discovery_etcd.erl"],
        outs = ["test/rabbit_peer_discovery_etcd.beam"],
        hdrs = ["include/rabbit_peer_discovery_etcd.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_peer_discovery_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbitmq_peer_discovery_etcd_app_beam",
        testonly = True,
        srcs = ["src/rabbitmq_peer_discovery_etcd_app.erl"],
        outs = ["test/rabbitmq_peer_discovery_etcd_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbitmq_peer_discovery_etcd_beam",
        testonly = True,
        srcs = ["src/rabbitmq_peer_discovery_etcd.erl"],
        outs = ["test/rabbitmq_peer_discovery_etcd.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbitmq_peer_discovery_etcd_sup_beam",
        testonly = True,
        srcs = ["src/rabbitmq_peer_discovery_etcd_sup.erl"],
        outs = ["test/rabbitmq_peer_discovery_etcd_sup.beam"],
        hdrs = ["include/rabbit_peer_discovery_etcd.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbitmq_peer_discovery_etcd_v3_client_beam",
        testonly = True,
        srcs = ["src/rabbitmq_peer_discovery_etcd_v3_client.erl"],
        outs = ["test/rabbitmq_peer_discovery_etcd_v3_client.beam"],
        hdrs = ["include/rabbit_peer_discovery_etcd.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/rabbit_peer_discovery_etcd.hrl", "src/rabbit_peer_discovery_etcd.erl", "src/rabbitmq_peer_discovery_etcd.erl", "src/rabbitmq_peer_discovery_etcd_app.erl", "src/rabbitmq_peer_discovery_etcd_sup.erl", "src/rabbitmq_peer_discovery_etcd_v3_client.erl"],
    )
