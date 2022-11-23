load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.ListAmqp10ConnectionsCommand.beam", "ebin/rabbit_amqp1_0.beam", "ebin/rabbit_amqp1_0_channel.beam", "ebin/rabbit_amqp1_0_incoming_link.beam", "ebin/rabbit_amqp1_0_link_util.beam", "ebin/rabbit_amqp1_0_message.beam", "ebin/rabbit_amqp1_0_outgoing_link.beam", "ebin/rabbit_amqp1_0_reader.beam", "ebin/rabbit_amqp1_0_session.beam", "ebin/rabbit_amqp1_0_session_process.beam", "ebin/rabbit_amqp1_0_session_sup.beam", "ebin/rabbit_amqp1_0_session_sup_sup.beam", "ebin/rabbit_amqp1_0_util.beam", "ebin/rabbit_amqp1_0_writer.beam"],
    )
    erlang_bytecode(
        name = "ebin_Elixir_RabbitMQ_CLI_Ctl_Commands_ListAmqp10ConnectionsCommand_beam",
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.ListAmqp10ConnectionsCommand.erl"],
        outs = ["ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.ListAmqp10ConnectionsCommand.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/rabbitmq_cli:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp1_0_beam",
        srcs = ["src/rabbit_amqp1_0.erl"],
        outs = ["ebin/rabbit_amqp1_0.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp1_0_channel_beam",
        srcs = ["src/rabbit_amqp1_0_channel.erl"],
        outs = ["ebin/rabbit_amqp1_0_channel.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp1_0_incoming_link_beam",
        srcs = ["src/rabbit_amqp1_0_incoming_link.erl"],
        outs = ["ebin/rabbit_amqp1_0_incoming_link.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp1_0_link_util_beam",
        srcs = ["src/rabbit_amqp1_0_link_util.erl"],
        outs = ["ebin/rabbit_amqp1_0_link_util.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp1_0_message_beam",
        srcs = ["src/rabbit_amqp1_0_message.erl"],
        outs = ["ebin/rabbit_amqp1_0_message.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp1_0_outgoing_link_beam",
        srcs = ["src/rabbit_amqp1_0_outgoing_link.erl"],
        outs = ["ebin/rabbit_amqp1_0_outgoing_link.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp1_0_reader_beam",
        srcs = ["src/rabbit_amqp1_0_reader.erl"],
        outs = ["ebin/rabbit_amqp1_0_reader.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp1_0_session_beam",
        srcs = ["src/rabbit_amqp1_0_session.erl"],
        outs = ["ebin/rabbit_amqp1_0_session.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp1_0_session_process_beam",
        srcs = ["src/rabbit_amqp1_0_session_process.erl"],
        outs = ["ebin/rabbit_amqp1_0_session_process.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/amqp_client:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp1_0_session_sup_beam",
        srcs = ["src/rabbit_amqp1_0_session_sup.erl"],
        outs = ["ebin/rabbit_amqp1_0_session_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp1_0_session_sup_sup_beam",
        srcs = ["src/rabbit_amqp1_0_session_sup_sup.erl"],
        outs = ["ebin/rabbit_amqp1_0_session_sup_sup.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp1_0_util_beam",
        srcs = ["src/rabbit_amqp1_0_util.erl"],
        outs = ["ebin/rabbit_amqp1_0_util.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp1_0_writer_beam",
        srcs = ["src/rabbit_amqp1_0_writer.erl"],
        outs = ["ebin/rabbit_amqp1_0_writer.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/rabbit_common:erlang_app"],
    )

def all_test_beam_files():
    erlang_bytecode(
        name = "test_Elixir_RabbitMQ_CLI_Ctl_Commands_ListAmqp10ConnectionsCommand_beam",
        testonly = True,
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.ListAmqp10ConnectionsCommand.erl"],
        outs = ["test/Elixir.RabbitMQ.CLI.Ctl.Commands.ListAmqp10ConnectionsCommand.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/rabbitmq_cli:erlang_app"],
    )
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/Elixir.RabbitMQ.CLI.Ctl.Commands.ListAmqp10ConnectionsCommand.beam", "test/rabbit_amqp1_0.beam", "test/rabbit_amqp1_0_channel.beam", "test/rabbit_amqp1_0_incoming_link.beam", "test/rabbit_amqp1_0_link_util.beam", "test/rabbit_amqp1_0_message.beam", "test/rabbit_amqp1_0_outgoing_link.beam", "test/rabbit_amqp1_0_reader.beam", "test/rabbit_amqp1_0_session.beam", "test/rabbit_amqp1_0_session_process.beam", "test/rabbit_amqp1_0_session_sup.beam", "test/rabbit_amqp1_0_session_sup_sup.beam", "test/rabbit_amqp1_0_util.beam", "test/rabbit_amqp1_0_writer.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqp1_0_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp1_0.erl"],
        outs = ["test/rabbit_amqp1_0.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_amqp1_0_channel_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp1_0_channel.erl"],
        outs = ["test/rabbit_amqp1_0_channel.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqp1_0_incoming_link_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp1_0_incoming_link.erl"],
        outs = ["test/rabbit_amqp1_0_incoming_link.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqp1_0_link_util_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp1_0_link_util.erl"],
        outs = ["test/rabbit_amqp1_0_link_util.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqp1_0_message_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp1_0_message.erl"],
        outs = ["test/rabbit_amqp1_0_message.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqp1_0_outgoing_link_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp1_0_outgoing_link.erl"],
        outs = ["test/rabbit_amqp1_0_outgoing_link.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqp1_0_reader_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp1_0_reader.erl"],
        outs = ["test/rabbit_amqp1_0_reader.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqp1_0_session_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp1_0_session.erl"],
        outs = ["test/rabbit_amqp1_0_session.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqp1_0_session_process_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp1_0_session_process.erl"],
        outs = ["test/rabbit_amqp1_0_session_process.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/amqp_client:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqp1_0_session_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp1_0_session_sup.erl"],
        outs = ["test/rabbit_amqp1_0_session_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqp1_0_session_sup_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp1_0_session_sup_sup.erl"],
        outs = ["test/rabbit_amqp1_0_session_sup_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_amqp1_0_util_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp1_0_util.erl"],
        outs = ["test/rabbit_amqp1_0_util.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqp1_0_writer_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp1_0_writer.erl"],
        outs = ["test/rabbit_amqp1_0_writer.beam"],
        hdrs = ["include/rabbit_amqp1_0.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/rabbit_common:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/rabbit_amqp1_0.hrl", "src/Elixir.RabbitMQ.CLI.Ctl.Commands.ListAmqp10ConnectionsCommand.erl", "src/rabbit_amqp1_0.erl", "src/rabbit_amqp1_0_channel.erl", "src/rabbit_amqp1_0_incoming_link.erl", "src/rabbit_amqp1_0_link_util.erl", "src/rabbit_amqp1_0_message.erl", "src/rabbit_amqp1_0_outgoing_link.erl", "src/rabbit_amqp1_0_reader.erl", "src/rabbit_amqp1_0_session.erl", "src/rabbit_amqp1_0_session_process.erl", "src/rabbit_amqp1_0_session_sup.erl", "src/rabbit_amqp1_0_session_sup_sup.erl", "src/rabbit_amqp1_0_util.erl", "src/rabbit_amqp1_0_writer.erl"],
    )
