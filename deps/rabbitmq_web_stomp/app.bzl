load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_web_stomp_app.beam", "ebin/rabbit_web_stomp_connection_sup.beam", "ebin/rabbit_web_stomp_handler.beam", "ebin/rabbit_web_stomp_internal_event_handler.beam", "ebin/rabbit_web_stomp_listener.beam", "ebin/rabbit_web_stomp_middleware.beam", "ebin/rabbit_web_stomp_stream_handler.beam", "ebin/rabbit_web_stomp_sup.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_stomp_app_beam",
        srcs = ["src/rabbit_web_stomp_app.erl"],
        outs = ["ebin/rabbit_web_stomp_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_stomp_connection_sup_beam",
        srcs = ["src/rabbit_web_stomp_connection_sup.erl"],
        outs = ["ebin/rabbit_web_stomp_connection_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@ranch//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_stomp_handler_beam",
        srcs = ["src/rabbit_web_stomp_handler.erl"],
        outs = ["ebin/rabbit_web_stomp_handler.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_stomp:erlang_app", "@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_stomp_internal_event_handler_beam",
        srcs = ["src/rabbit_web_stomp_internal_event_handler.erl"],
        outs = ["ebin/rabbit_web_stomp_internal_event_handler.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_stomp_listener_beam",
        srcs = ["src/rabbit_web_stomp_listener.erl"],
        outs = ["ebin/rabbit_web_stomp_listener.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_stomp:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_stomp_middleware_beam",
        srcs = ["src/rabbit_web_stomp_middleware.erl"],
        outs = ["ebin/rabbit_web_stomp_middleware.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_stomp_stream_handler_beam",
        srcs = ["src/rabbit_web_stomp_stream_handler.erl"],
        outs = ["ebin/rabbit_web_stomp_stream_handler.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_stomp_sup_beam",
        srcs = ["src/rabbit_web_stomp_sup.erl"],
        outs = ["ebin/rabbit_web_stomp_sup.beam"],
        erlc_opts = "//:erlc_opts",
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_web_stomp_app.beam", "test/rabbit_web_stomp_connection_sup.beam", "test/rabbit_web_stomp_handler.beam", "test/rabbit_web_stomp_internal_event_handler.beam", "test/rabbit_web_stomp_listener.beam", "test/rabbit_web_stomp_middleware.beam", "test/rabbit_web_stomp_stream_handler.beam", "test/rabbit_web_stomp_sup.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_web_stomp_app_beam",
        testonly = True,
        srcs = ["src/rabbit_web_stomp_app.erl"],
        outs = ["test/rabbit_web_stomp_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_web_stomp_connection_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_web_stomp_connection_sup.erl"],
        outs = ["test/rabbit_web_stomp_connection_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@ranch//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_web_stomp_handler_beam",
        testonly = True,
        srcs = ["src/rabbit_web_stomp_handler.erl"],
        outs = ["test/rabbit_web_stomp_handler.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_stomp:erlang_app", "@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_web_stomp_internal_event_handler_beam",
        testonly = True,
        srcs = ["src/rabbit_web_stomp_internal_event_handler.erl"],
        outs = ["test/rabbit_web_stomp_internal_event_handler.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_web_stomp_listener_beam",
        testonly = True,
        srcs = ["src/rabbit_web_stomp_listener.erl"],
        outs = ["test/rabbit_web_stomp_listener.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_stomp:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_web_stomp_middleware_beam",
        testonly = True,
        srcs = ["src/rabbit_web_stomp_middleware.erl"],
        outs = ["test/rabbit_web_stomp_middleware.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_web_stomp_stream_handler_beam",
        testonly = True,
        srcs = ["src/rabbit_web_stomp_stream_handler.erl"],
        outs = ["test/rabbit_web_stomp_stream_handler.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_web_stomp_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_web_stomp_sup.erl"],
        outs = ["test/rabbit_web_stomp_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/rabbit_web_stomp_app.erl", "src/rabbit_web_stomp_connection_sup.erl", "src/rabbit_web_stomp_handler.erl", "src/rabbit_web_stomp_internal_event_handler.erl", "src/rabbit_web_stomp_listener.erl", "src/rabbit_web_stomp_middleware.erl", "src/rabbit_web_stomp_stream_handler.erl", "src/rabbit_web_stomp_sup.erl"],
    )
