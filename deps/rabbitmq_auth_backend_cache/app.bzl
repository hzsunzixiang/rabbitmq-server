load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_auth_backend_cache.beam", "ebin/rabbit_auth_backend_cache_app.beam", "ebin/rabbit_auth_cache.beam", "ebin/rabbit_auth_cache_dict.beam", "ebin/rabbit_auth_cache_ets.beam", "ebin/rabbit_auth_cache_ets_segmented.beam", "ebin/rabbit_auth_cache_ets_segmented_stateless.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_backend_cache_app_beam",
        srcs = ["src/rabbit_auth_backend_cache_app.erl"],
        outs = ["ebin/rabbit_auth_backend_cache_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_backend_cache_beam",
        srcs = ["src/rabbit_auth_backend_cache.erl"],
        outs = ["ebin/rabbit_auth_backend_cache.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_cache_beam",
        srcs = ["src/rabbit_auth_cache.erl"],
        outs = ["ebin/rabbit_auth_cache.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_cache_dict_beam",
        srcs = ["src/rabbit_auth_cache_dict.erl"],
        outs = ["ebin/rabbit_auth_cache_dict.beam"],
        hdrs = ["include/rabbit_auth_backend_cache.hrl"],
        beam = ["ebin/rabbit_auth_cache.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_cache_ets_beam",
        srcs = ["src/rabbit_auth_cache_ets.erl"],
        outs = ["ebin/rabbit_auth_cache_ets.beam"],
        hdrs = ["include/rabbit_auth_backend_cache.hrl"],
        beam = ["ebin/rabbit_auth_cache.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_cache_ets_segmented_beam",
        srcs = ["src/rabbit_auth_cache_ets_segmented.erl"],
        outs = ["ebin/rabbit_auth_cache_ets_segmented.beam"],
        hdrs = ["include/rabbit_auth_backend_cache.hrl"],
        beam = ["ebin/rabbit_auth_cache.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_cache_ets_segmented_stateless_beam",
        srcs = ["src/rabbit_auth_cache_ets_segmented_stateless.erl"],
        outs = ["ebin/rabbit_auth_cache_ets_segmented_stateless.beam"],
        hdrs = ["include/rabbit_auth_backend_cache.hrl"],
        beam = ["ebin/rabbit_auth_cache.beam"],
        erlc_opts = "//:erlc_opts",
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_auth_backend_cache.beam", "test/rabbit_auth_backend_cache_app.beam", "test/rabbit_auth_cache.beam", "test/rabbit_auth_cache_dict.beam", "test/rabbit_auth_cache_ets.beam", "test/rabbit_auth_cache_ets_segmented.beam", "test/rabbit_auth_cache_ets_segmented_stateless.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_auth_backend_cache_app_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_backend_cache_app.erl"],
        outs = ["test/rabbit_auth_backend_cache_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_auth_backend_cache_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_backend_cache.erl"],
        outs = ["test/rabbit_auth_backend_cache.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_auth_cache_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_cache.erl"],
        outs = ["test/rabbit_auth_cache.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_auth_cache_dict_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_cache_dict.erl"],
        outs = ["test/rabbit_auth_cache_dict.beam"],
        hdrs = ["include/rabbit_auth_backend_cache.hrl"],
        beam = ["ebin/rabbit_auth_cache.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_auth_cache_ets_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_cache_ets.erl"],
        outs = ["test/rabbit_auth_cache_ets.beam"],
        hdrs = ["include/rabbit_auth_backend_cache.hrl"],
        beam = ["ebin/rabbit_auth_cache.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_auth_cache_ets_segmented_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_cache_ets_segmented.erl"],
        outs = ["test/rabbit_auth_cache_ets_segmented.beam"],
        hdrs = ["include/rabbit_auth_backend_cache.hrl"],
        beam = ["ebin/rabbit_auth_cache.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_auth_cache_ets_segmented_stateless_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_cache_ets_segmented_stateless.erl"],
        outs = ["test/rabbit_auth_cache_ets_segmented_stateless.beam"],
        hdrs = ["include/rabbit_auth_backend_cache.hrl"],
        beam = ["ebin/rabbit_auth_cache.beam"],
        erlc_opts = "//:test_erlc_opts",
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/rabbit_auth_backend_cache.hrl", "src/rabbit_auth_backend_cache.erl", "src/rabbit_auth_backend_cache_app.erl", "src/rabbit_auth_cache.erl", "src/rabbit_auth_cache_dict.erl", "src/rabbit_auth_cache_ets.erl", "src/rabbit_auth_cache_ets_segmented.erl", "src/rabbit_auth_cache_ets_segmented_stateless.erl"],
    )
