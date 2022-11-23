load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/amqp10_client.beam", "ebin/amqp10_client_app.beam", "ebin/amqp10_client_connection.beam", "ebin/amqp10_client_connection_sup.beam", "ebin/amqp10_client_connections_sup.beam", "ebin/amqp10_client_frame_reader.beam", "ebin/amqp10_client_session.beam", "ebin/amqp10_client_sessions_sup.beam", "ebin/amqp10_client_sup.beam", "ebin/amqp10_client_types.beam", "ebin/amqp10_msg.beam"],
    )
    erlang_bytecode(
        name = "ebin_amqp10_client_app_beam",
        srcs = ["src/amqp10_client_app.erl"],
        outs = ["ebin/amqp10_client_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_amqp10_client_beam",
        srcs = ["src/amqp10_client.erl"],
        outs = ["ebin/amqp10_client.beam"],
        hdrs = ["src/amqp10_client.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp10_client_connection_beam",
        srcs = ["src/amqp10_client_connection.erl"],
        outs = ["ebin/amqp10_client_connection.beam"],
        hdrs = ["src/amqp10_client.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp10_client_connection_sup_beam",
        srcs = ["src/amqp10_client_connection_sup.erl"],
        outs = ["ebin/amqp10_client_connection_sup.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_amqp10_client_connections_sup_beam",
        srcs = ["src/amqp10_client_connections_sup.erl"],
        outs = ["ebin/amqp10_client_connections_sup.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_amqp10_client_frame_reader_beam",
        srcs = ["src/amqp10_client_frame_reader.erl"],
        outs = ["ebin/amqp10_client_frame_reader.beam"],
        hdrs = ["src/amqp10_client.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp10_client_session_beam",
        srcs = ["src/amqp10_client_session.erl"],
        outs = ["ebin/amqp10_client_session.beam"],
        hdrs = ["src/amqp10_client.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp10_client_sessions_sup_beam",
        srcs = ["src/amqp10_client_sessions_sup.erl"],
        outs = ["ebin/amqp10_client_sessions_sup.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_amqp10_client_sup_beam",
        srcs = ["src/amqp10_client_sup.erl"],
        outs = ["ebin/amqp10_client_sup.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_amqp10_client_types_beam",
        srcs = ["src/amqp10_client_types.erl"],
        outs = ["ebin/amqp10_client_types.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp10_msg_beam",
        srcs = ["src/amqp10_msg.erl"],
        outs = ["ebin/amqp10_msg.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app"],
    )

def all_test_beam_files():
    erlang_bytecode(
        name = "test_amqp10_client_app_beam",
        testonly = True,
        srcs = ["src/amqp10_client_app.erl"],
        outs = ["test/amqp10_client_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_amqp10_client_beam",
        testonly = True,
        srcs = ["src/amqp10_client.erl"],
        outs = ["test/amqp10_client.beam"],
        hdrs = ["src/amqp10_client.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp10_client_connection_beam",
        testonly = True,
        srcs = ["src/amqp10_client_connection.erl"],
        outs = ["test/amqp10_client_connection.beam"],
        hdrs = ["src/amqp10_client.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp10_client_connection_sup_beam",
        testonly = True,
        srcs = ["src/amqp10_client_connection_sup.erl"],
        outs = ["test/amqp10_client_connection_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_amqp10_client_connections_sup_beam",
        testonly = True,
        srcs = ["src/amqp10_client_connections_sup.erl"],
        outs = ["test/amqp10_client_connections_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_amqp10_client_frame_reader_beam",
        testonly = True,
        srcs = ["src/amqp10_client_frame_reader.erl"],
        outs = ["test/amqp10_client_frame_reader.beam"],
        hdrs = ["src/amqp10_client.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp10_client_session_beam",
        testonly = True,
        srcs = ["src/amqp10_client_session.erl"],
        outs = ["test/amqp10_client_session.beam"],
        hdrs = ["src/amqp10_client.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp10_client_sessions_sup_beam",
        testonly = True,
        srcs = ["src/amqp10_client_sessions_sup.erl"],
        outs = ["test/amqp10_client_sessions_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_amqp10_client_sup_beam",
        testonly = True,
        srcs = ["src/amqp10_client_sup.erl"],
        outs = ["test/amqp10_client_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_amqp10_client_types_beam",
        testonly = True,
        srcs = ["src/amqp10_client_types.erl"],
        outs = ["test/amqp10_client_types.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp10_msg_beam",
        testonly = True,
        srcs = ["src/amqp10_msg.erl"],
        outs = ["test/amqp10_msg.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app"],
    )
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/amqp10_client.beam", "test/amqp10_client_app.beam", "test/amqp10_client_connection.beam", "test/amqp10_client_connection_sup.beam", "test/amqp10_client_connections_sup.beam", "test/amqp10_client_frame_reader.beam", "test/amqp10_client_session.beam", "test/amqp10_client_sessions_sup.beam", "test/amqp10_client_sup.beam", "test/amqp10_client_types.beam", "test/amqp10_msg.beam"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/amqp10_client.erl", "src/amqp10_client.hrl", "src/amqp10_client_app.erl", "src/amqp10_client_connection.erl", "src/amqp10_client_connection_sup.erl", "src/amqp10_client_connections_sup.erl", "src/amqp10_client_frame_reader.erl", "src/amqp10_client_session.erl", "src/amqp10_client_sessions_sup.erl", "src/amqp10_client_sup.erl", "src/amqp10_client_types.erl", "src/amqp10_msg.erl"],
    )
