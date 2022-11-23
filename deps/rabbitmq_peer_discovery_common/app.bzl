load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_peer_discovery_cleanup.beam", "ebin/rabbit_peer_discovery_common_app.beam", "ebin/rabbit_peer_discovery_common_sup.beam", "ebin/rabbit_peer_discovery_config.beam", "ebin/rabbit_peer_discovery_httpc.beam", "ebin/rabbit_peer_discovery_util.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_peer_discovery_cleanup_beam",
        srcs = ["src/rabbit_peer_discovery_cleanup.erl"],
        outs = ["ebin/rabbit_peer_discovery_cleanup.beam"],
        hdrs = ["include/rabbit_peer_discovery.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_peer_discovery_common_app_beam",
        srcs = ["src/rabbit_peer_discovery_common_app.erl"],
        outs = ["ebin/rabbit_peer_discovery_common_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_peer_discovery_common_sup_beam",
        srcs = ["src/rabbit_peer_discovery_common_sup.erl"],
        outs = ["ebin/rabbit_peer_discovery_common_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_peer_discovery_config_beam",
        srcs = ["src/rabbit_peer_discovery_config.erl"],
        outs = ["ebin/rabbit_peer_discovery_config.beam"],
        hdrs = ["include/rabbit_peer_discovery.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_peer_discovery_httpc_beam",
        srcs = ["src/rabbit_peer_discovery_httpc.erl"],
        outs = ["ebin/rabbit_peer_discovery_httpc.beam"],
        hdrs = ["include/rabbit_peer_discovery.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_peer_discovery_util_beam",
        srcs = ["src/rabbit_peer_discovery_util.erl"],
        outs = ["ebin/rabbit_peer_discovery_util.beam"],
        hdrs = ["include/rabbit_peer_discovery.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_peer_discovery_cleanup.beam", "test/rabbit_peer_discovery_common_app.beam", "test/rabbit_peer_discovery_common_sup.beam", "test/rabbit_peer_discovery_config.beam", "test/rabbit_peer_discovery_httpc.beam", "test/rabbit_peer_discovery_util.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_peer_discovery_cleanup_beam",
        testonly = True,
        srcs = ["src/rabbit_peer_discovery_cleanup.erl"],
        outs = ["test/rabbit_peer_discovery_cleanup.beam"],
        hdrs = ["include/rabbit_peer_discovery.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_peer_discovery_common_app_beam",
        testonly = True,
        srcs = ["src/rabbit_peer_discovery_common_app.erl"],
        outs = ["test/rabbit_peer_discovery_common_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_peer_discovery_common_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_peer_discovery_common_sup.erl"],
        outs = ["test/rabbit_peer_discovery_common_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_peer_discovery_config_beam",
        testonly = True,
        srcs = ["src/rabbit_peer_discovery_config.erl"],
        outs = ["test/rabbit_peer_discovery_config.beam"],
        hdrs = ["include/rabbit_peer_discovery.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_peer_discovery_httpc_beam",
        testonly = True,
        srcs = ["src/rabbit_peer_discovery_httpc.erl"],
        outs = ["test/rabbit_peer_discovery_httpc.beam"],
        hdrs = ["include/rabbit_peer_discovery.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_peer_discovery_util_beam",
        testonly = True,
        srcs = ["src/rabbit_peer_discovery_util.erl"],
        outs = ["test/rabbit_peer_discovery_util.beam"],
        hdrs = ["include/rabbit_peer_discovery.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/rabbit_peer_discovery.hrl", "src/rabbit_peer_discovery_cleanup.erl", "src/rabbit_peer_discovery_common_app.erl", "src/rabbit_peer_discovery_common_sup.erl", "src/rabbit_peer_discovery_config.erl", "src/rabbit_peer_discovery_httpc.erl", "src/rabbit_peer_discovery_util.erl"],
    )
