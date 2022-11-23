load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_cowboy_middleware.beam", "ebin/rabbit_cowboy_redirect.beam", "ebin/rabbit_cowboy_stream_h.beam", "ebin/rabbit_web_dispatch.beam", "ebin/rabbit_web_dispatch_app.beam", "ebin/rabbit_web_dispatch_listing_handler.beam", "ebin/rabbit_web_dispatch_registry.beam", "ebin/rabbit_web_dispatch_sup.beam", "ebin/rabbit_web_dispatch_util.beam", "ebin/webmachine_log.beam", "ebin/webmachine_log_handler.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_cowboy_middleware_beam",
        srcs = ["src/rabbit_cowboy_middleware.erl"],
        outs = ["ebin/rabbit_cowboy_middleware.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_cowboy_redirect_beam",
        srcs = ["src/rabbit_cowboy_redirect.erl"],
        outs = ["ebin/rabbit_cowboy_redirect.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_cowboy_stream_h_beam",
        srcs = ["src/rabbit_cowboy_stream_h.erl"],
        outs = ["ebin/rabbit_cowboy_stream_h.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_dispatch_app_beam",
        srcs = ["src/rabbit_web_dispatch_app.erl"],
        outs = ["ebin/rabbit_web_dispatch_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_dispatch_beam",
        srcs = ["src/rabbit_web_dispatch.erl"],
        outs = ["ebin/rabbit_web_dispatch.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_dispatch_listing_handler_beam",
        srcs = ["src/rabbit_web_dispatch_listing_handler.erl"],
        outs = ["ebin/rabbit_web_dispatch_listing_handler.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_dispatch_registry_beam",
        srcs = ["src/rabbit_web_dispatch_registry.erl"],
        outs = ["ebin/rabbit_web_dispatch_registry.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_dispatch_sup_beam",
        srcs = ["src/rabbit_web_dispatch_sup.erl"],
        outs = ["ebin/rabbit_web_dispatch_sup.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_dispatch_util_beam",
        srcs = ["src/rabbit_web_dispatch_util.erl"],
        outs = ["ebin/rabbit_web_dispatch_util.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_webmachine_log_beam",
        srcs = ["src/webmachine_log.erl"],
        outs = ["ebin/webmachine_log.beam"],
        hdrs = ["src/webmachine_logger.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_webmachine_log_handler_beam",
        srcs = ["src/webmachine_log_handler.erl"],
        outs = ["ebin/webmachine_log_handler.beam"],
        hdrs = ["src/webmachine_logger.hrl"],
        erlc_opts = "//:erlc_opts",
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_cowboy_middleware.beam", "test/rabbit_cowboy_redirect.beam", "test/rabbit_cowboy_stream_h.beam", "test/rabbit_web_dispatch.beam", "test/rabbit_web_dispatch_app.beam", "test/rabbit_web_dispatch_listing_handler.beam", "test/rabbit_web_dispatch_registry.beam", "test/rabbit_web_dispatch_sup.beam", "test/rabbit_web_dispatch_util.beam", "test/webmachine_log.beam", "test/webmachine_log_handler.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_cowboy_middleware_beam",
        testonly = True,
        srcs = ["src/rabbit_cowboy_middleware.erl"],
        outs = ["test/rabbit_cowboy_middleware.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_cowboy_redirect_beam",
        testonly = True,
        srcs = ["src/rabbit_cowboy_redirect.erl"],
        outs = ["test/rabbit_cowboy_redirect.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_cowboy_stream_h_beam",
        testonly = True,
        srcs = ["src/rabbit_cowboy_stream_h.erl"],
        outs = ["test/rabbit_cowboy_stream_h.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_web_dispatch_app_beam",
        testonly = True,
        srcs = ["src/rabbit_web_dispatch_app.erl"],
        outs = ["test/rabbit_web_dispatch_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_web_dispatch_beam",
        testonly = True,
        srcs = ["src/rabbit_web_dispatch.erl"],
        outs = ["test/rabbit_web_dispatch.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_web_dispatch_listing_handler_beam",
        testonly = True,
        srcs = ["src/rabbit_web_dispatch_listing_handler.erl"],
        outs = ["test/rabbit_web_dispatch_listing_handler.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_web_dispatch_registry_beam",
        testonly = True,
        srcs = ["src/rabbit_web_dispatch_registry.erl"],
        outs = ["test/rabbit_web_dispatch_registry.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_web_dispatch_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_web_dispatch_sup.erl"],
        outs = ["test/rabbit_web_dispatch_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_web_dispatch_util_beam",
        testonly = True,
        srcs = ["src/rabbit_web_dispatch_util.erl"],
        outs = ["test/rabbit_web_dispatch_util.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_webmachine_log_beam",
        testonly = True,
        srcs = ["src/webmachine_log.erl"],
        outs = ["test/webmachine_log.beam"],
        hdrs = ["src/webmachine_logger.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_webmachine_log_handler_beam",
        testonly = True,
        srcs = ["src/webmachine_log_handler.erl"],
        outs = ["test/webmachine_log_handler.beam"],
        hdrs = ["src/webmachine_logger.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/rabbit_cowboy_middleware.erl", "src/rabbit_cowboy_redirect.erl", "src/rabbit_cowboy_stream_h.erl", "src/rabbit_web_dispatch.erl", "src/rabbit_web_dispatch_app.erl", "src/rabbit_web_dispatch_listing_handler.erl", "src/rabbit_web_dispatch_registry.erl", "src/rabbit_web_dispatch_sup.erl", "src/rabbit_web_dispatch_util.erl", "src/webmachine_log.erl", "src/webmachine_log_handler.erl", "src/webmachine_logger.hrl"],
    )
