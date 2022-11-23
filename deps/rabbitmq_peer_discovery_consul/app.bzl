load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_peer_discovery_consul.beam", "ebin/rabbitmq_peer_discovery_consul.beam", "ebin/rabbitmq_peer_discovery_consul_app.beam", "ebin/rabbitmq_peer_discovery_consul_health_check_helper.beam", "ebin/rabbitmq_peer_discovery_consul_sup.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_peer_discovery_consul_beam",
        srcs = ["src/rabbit_peer_discovery_consul.erl"],
        outs = ["ebin/rabbit_peer_discovery_consul.beam"],
        hdrs = ["include/rabbit_peer_discovery_consul.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_peer_discovery_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_peer_discovery_consul_app_beam",
        srcs = ["src/rabbitmq_peer_discovery_consul_app.erl"],
        outs = ["ebin/rabbitmq_peer_discovery_consul_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_peer_discovery_consul_beam",
        srcs = ["src/rabbitmq_peer_discovery_consul.erl"],
        outs = ["ebin/rabbitmq_peer_discovery_consul.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_peer_discovery_consul_health_check_helper_beam",
        srcs = ["src/rabbitmq_peer_discovery_consul_health_check_helper.erl"],
        outs = ["ebin/rabbitmq_peer_discovery_consul_health_check_helper.beam"],
        hdrs = ["include/rabbit_peer_discovery_consul.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_peer_discovery_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_peer_discovery_consul_sup_beam",
        srcs = ["src/rabbitmq_peer_discovery_consul_sup.erl"],
        outs = ["ebin/rabbitmq_peer_discovery_consul_sup.beam"],
        hdrs = ["include/rabbit_peer_discovery_consul.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_peer_discovery_consul.beam", "test/rabbitmq_peer_discovery_consul.beam", "test/rabbitmq_peer_discovery_consul_app.beam", "test/rabbitmq_peer_discovery_consul_health_check_helper.beam", "test/rabbitmq_peer_discovery_consul_sup.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_peer_discovery_consul_beam",
        testonly = True,
        srcs = ["src/rabbit_peer_discovery_consul.erl"],
        outs = ["test/rabbit_peer_discovery_consul.beam"],
        hdrs = ["include/rabbit_peer_discovery_consul.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_peer_discovery_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbitmq_peer_discovery_consul_app_beam",
        testonly = True,
        srcs = ["src/rabbitmq_peer_discovery_consul_app.erl"],
        outs = ["test/rabbitmq_peer_discovery_consul_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbitmq_peer_discovery_consul_beam",
        testonly = True,
        srcs = ["src/rabbitmq_peer_discovery_consul.erl"],
        outs = ["test/rabbitmq_peer_discovery_consul.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbitmq_peer_discovery_consul_health_check_helper_beam",
        testonly = True,
        srcs = ["src/rabbitmq_peer_discovery_consul_health_check_helper.erl"],
        outs = ["test/rabbitmq_peer_discovery_consul_health_check_helper.beam"],
        hdrs = ["include/rabbit_peer_discovery_consul.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_peer_discovery_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbitmq_peer_discovery_consul_sup_beam",
        testonly = True,
        srcs = ["src/rabbitmq_peer_discovery_consul_sup.erl"],
        outs = ["test/rabbitmq_peer_discovery_consul_sup.beam"],
        hdrs = ["include/rabbit_peer_discovery_consul.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/rabbit_peer_discovery_consul.hrl", "src/rabbit_peer_discovery_consul.erl", "src/rabbitmq_peer_discovery_consul.erl", "src/rabbitmq_peer_discovery_consul_app.erl", "src/rabbitmq_peer_discovery_consul_health_check_helper.erl", "src/rabbitmq_peer_discovery_consul_sup.erl"],
    )
