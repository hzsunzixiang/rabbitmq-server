load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_web_mqtt_app.beam", "ebin/rabbit_web_mqtt_connection_info.beam", "ebin/rabbit_web_mqtt_connection_sup.beam", "ebin/rabbit_web_mqtt_handler.beam", "ebin/rabbit_web_mqtt_middleware.beam", "ebin/rabbit_web_mqtt_stream_handler.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_mqtt_app_beam",
        srcs = ["src/rabbit_web_mqtt_app.erl"],
        outs = ["ebin/rabbit_web_mqtt_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_mqtt_connection_info_beam",
        srcs = ["src/rabbit_web_mqtt_connection_info.erl"],
        outs = ["ebin/rabbit_web_mqtt_connection_info.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_mqtt_connection_sup_beam",
        srcs = ["src/rabbit_web_mqtt_connection_sup.erl"],
        outs = ["ebin/rabbit_web_mqtt_connection_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@ranch//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_mqtt_handler_beam",
        srcs = ["src/rabbit_web_mqtt_handler.erl"],
        outs = ["ebin/rabbit_web_mqtt_handler.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_mqtt_middleware_beam",
        srcs = ["src/rabbit_web_mqtt_middleware.erl"],
        outs = ["ebin/rabbit_web_mqtt_middleware.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_mqtt_stream_handler_beam",
        srcs = ["src/rabbit_web_mqtt_stream_handler.erl"],
        outs = ["ebin/rabbit_web_mqtt_stream_handler.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_web_mqtt_app.beam", "test/rabbit_web_mqtt_connection_info.beam", "test/rabbit_web_mqtt_connection_sup.beam", "test/rabbit_web_mqtt_handler.beam", "test/rabbit_web_mqtt_middleware.beam", "test/rabbit_web_mqtt_stream_handler.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_web_mqtt_app_beam",
        testonly = True,
        srcs = ["src/rabbit_web_mqtt_app.erl"],
        outs = ["test/rabbit_web_mqtt_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_web_mqtt_connection_info_beam",
        testonly = True,
        srcs = ["src/rabbit_web_mqtt_connection_info.erl"],
        outs = ["test/rabbit_web_mqtt_connection_info.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_web_mqtt_connection_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_web_mqtt_connection_sup.erl"],
        outs = ["test/rabbit_web_mqtt_connection_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@ranch//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_web_mqtt_handler_beam",
        testonly = True,
        srcs = ["src/rabbit_web_mqtt_handler.erl"],
        outs = ["test/rabbit_web_mqtt_handler.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_web_mqtt_middleware_beam",
        testonly = True,
        srcs = ["src/rabbit_web_mqtt_middleware.erl"],
        outs = ["test/rabbit_web_mqtt_middleware.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_web_mqtt_stream_handler_beam",
        testonly = True,
        srcs = ["src/rabbit_web_mqtt_stream_handler.erl"],
        outs = ["test/rabbit_web_mqtt_stream_handler.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/rabbit_web_mqtt_app.erl", "src/rabbit_web_mqtt_connection_info.erl", "src/rabbit_web_mqtt_connection_sup.erl", "src/rabbit_web_mqtt_handler.erl", "src/rabbit_web_mqtt_middleware.erl", "src/rabbit_web_mqtt_stream_handler.erl"],
    )
