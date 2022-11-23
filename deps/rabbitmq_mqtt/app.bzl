load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.DecommissionMqttNodeCommand.beam", "ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.ListMqttConnectionsCommand.beam", "ebin/mqtt_machine.beam", "ebin/mqtt_machine_v0.beam", "ebin/mqtt_node.beam", "ebin/rabbit_mqtt.beam", "ebin/rabbit_mqtt_collector.beam", "ebin/rabbit_mqtt_connection_info.beam", "ebin/rabbit_mqtt_connection_sup.beam", "ebin/rabbit_mqtt_frame.beam", "ebin/rabbit_mqtt_internal_event_handler.beam", "ebin/rabbit_mqtt_processor.beam", "ebin/rabbit_mqtt_reader.beam", "ebin/rabbit_mqtt_retained_msg_store.beam", "ebin/rabbit_mqtt_retained_msg_store_dets.beam", "ebin/rabbit_mqtt_retained_msg_store_ets.beam", "ebin/rabbit_mqtt_retained_msg_store_noop.beam", "ebin/rabbit_mqtt_retainer.beam", "ebin/rabbit_mqtt_retainer_sup.beam", "ebin/rabbit_mqtt_sup.beam", "ebin/rabbit_mqtt_util.beam"],
    )
    erlang_bytecode(
        name = "ebin_Elixir_RabbitMQ_CLI_Ctl_Commands_DecommissionMqttNodeCommand_beam",
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.DecommissionMqttNodeCommand.erl"],
        outs = ["ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.DecommissionMqttNodeCommand.beam"],
        hdrs = ["include/rabbit_mqtt.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_Elixir_RabbitMQ_CLI_Ctl_Commands_ListMqttConnectionsCommand_beam",
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.ListMqttConnectionsCommand.erl"],
        outs = ["ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.ListMqttConnectionsCommand.beam"],
        hdrs = ["include/rabbit_mqtt.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_mqtt_machine_beam",
        srcs = ["src/mqtt_machine.erl"],
        outs = ["ebin/mqtt_machine.beam"],
        hdrs = ["include/mqtt_machine.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["@ra//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_mqtt_machine_v0_beam",
        srcs = ["src/mqtt_machine_v0.erl"],
        outs = ["ebin/mqtt_machine_v0.beam"],
        hdrs = ["include/mqtt_machine_v0.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["@ra//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_mqtt_node_beam",
        srcs = ["src/mqtt_node.erl"],
        outs = ["ebin/mqtt_node.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_beam",
        srcs = ["src/rabbit_mqtt.erl"],
        outs = ["ebin/rabbit_mqtt.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_collector_beam",
        srcs = ["src/rabbit_mqtt_collector.erl"],
        outs = ["ebin/rabbit_mqtt_collector.beam"],
        hdrs = ["include/mqtt_machine.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_connection_info_beam",
        srcs = ["src/rabbit_mqtt_connection_info.erl"],
        outs = ["ebin/rabbit_mqtt_connection_info.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_connection_sup_beam",
        srcs = ["src/rabbit_mqtt_connection_sup.erl"],
        outs = ["ebin/rabbit_mqtt_connection_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@ranch//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_frame_beam",
        srcs = ["src/rabbit_mqtt_frame.erl"],
        outs = ["ebin/rabbit_mqtt_frame.beam"],
        hdrs = ["include/rabbit_mqtt_frame.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_internal_event_handler_beam",
        srcs = ["src/rabbit_mqtt_internal_event_handler.erl"],
        outs = ["ebin/rabbit_mqtt_internal_event_handler.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_processor_beam",
        srcs = ["src/rabbit_mqtt_processor.erl"],
        outs = ["ebin/rabbit_mqtt_processor.beam"],
        hdrs = ["include/rabbit_mqtt.hrl", "include/rabbit_mqtt_frame.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_reader_beam",
        srcs = ["src/rabbit_mqtt_reader.erl"],
        outs = ["ebin/rabbit_mqtt_reader.beam"],
        hdrs = ["include/rabbit_mqtt.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_retained_msg_store_beam",
        srcs = ["src/rabbit_mqtt_retained_msg_store.erl"],
        outs = ["ebin/rabbit_mqtt_retained_msg_store.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_retained_msg_store_dets_beam",
        srcs = ["src/rabbit_mqtt_retained_msg_store_dets.erl"],
        outs = ["ebin/rabbit_mqtt_retained_msg_store_dets.beam"],
        hdrs = ["include/rabbit_mqtt.hrl"],
        beam = ["ebin/rabbit_mqtt_retained_msg_store.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_retained_msg_store_ets_beam",
        srcs = ["src/rabbit_mqtt_retained_msg_store_ets.erl"],
        outs = ["ebin/rabbit_mqtt_retained_msg_store_ets.beam"],
        hdrs = ["include/rabbit_mqtt.hrl"],
        beam = ["ebin/rabbit_mqtt_retained_msg_store.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_retained_msg_store_noop_beam",
        srcs = ["src/rabbit_mqtt_retained_msg_store_noop.erl"],
        outs = ["ebin/rabbit_mqtt_retained_msg_store_noop.beam"],
        hdrs = ["include/rabbit_mqtt.hrl"],
        beam = ["ebin/rabbit_mqtt_retained_msg_store.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_retainer_beam",
        srcs = ["src/rabbit_mqtt_retainer.erl"],
        outs = ["ebin/rabbit_mqtt_retainer.beam"],
        hdrs = ["include/rabbit_mqtt.hrl", "include/rabbit_mqtt_frame.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_retainer_sup_beam",
        srcs = ["src/rabbit_mqtt_retainer_sup.erl"],
        outs = ["ebin/rabbit_mqtt_retainer_sup.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_sup_beam",
        srcs = ["src/rabbit_mqtt_sup.erl"],
        outs = ["ebin/rabbit_mqtt_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mqtt_util_beam",
        srcs = ["src/rabbit_mqtt_util.erl"],
        outs = ["ebin/rabbit_mqtt_util.beam"],
        hdrs = ["include/rabbit_mqtt.hrl"],
        erlc_opts = "//:erlc_opts",
    )

def all_test_beam_files():
    erlang_bytecode(
        name = "test_Elixir_RabbitMQ_CLI_Ctl_Commands_DecommissionMqttNodeCommand_beam",
        testonly = True,
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.DecommissionMqttNodeCommand.erl"],
        outs = ["test/Elixir.RabbitMQ.CLI.Ctl.Commands.DecommissionMqttNodeCommand.beam"],
        hdrs = ["include/rabbit_mqtt.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    erlang_bytecode(
        name = "test_Elixir_RabbitMQ_CLI_Ctl_Commands_ListMqttConnectionsCommand_beam",
        testonly = True,
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.ListMqttConnectionsCommand.erl"],
        outs = ["test/Elixir.RabbitMQ.CLI.Ctl.Commands.ListMqttConnectionsCommand.beam"],
        hdrs = ["include/rabbit_mqtt.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/Elixir.RabbitMQ.CLI.Ctl.Commands.DecommissionMqttNodeCommand.beam", "test/Elixir.RabbitMQ.CLI.Ctl.Commands.ListMqttConnectionsCommand.beam", "test/mqtt_machine.beam", "test/mqtt_machine_v0.beam", "test/mqtt_node.beam", "test/rabbit_mqtt.beam", "test/rabbit_mqtt_collector.beam", "test/rabbit_mqtt_connection_info.beam", "test/rabbit_mqtt_connection_sup.beam", "test/rabbit_mqtt_frame.beam", "test/rabbit_mqtt_internal_event_handler.beam", "test/rabbit_mqtt_processor.beam", "test/rabbit_mqtt_reader.beam", "test/rabbit_mqtt_retained_msg_store.beam", "test/rabbit_mqtt_retained_msg_store_dets.beam", "test/rabbit_mqtt_retained_msg_store_ets.beam", "test/rabbit_mqtt_retained_msg_store_noop.beam", "test/rabbit_mqtt_retainer.beam", "test/rabbit_mqtt_retainer_sup.beam", "test/rabbit_mqtt_sup.beam", "test/rabbit_mqtt_util.beam"],
    )
    erlang_bytecode(
        name = "test_mqtt_machine_beam",
        testonly = True,
        srcs = ["src/mqtt_machine.erl"],
        outs = ["test/mqtt_machine.beam"],
        hdrs = ["include/mqtt_machine.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@ra//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_mqtt_machine_v0_beam",
        testonly = True,
        srcs = ["src/mqtt_machine_v0.erl"],
        outs = ["test/mqtt_machine_v0.beam"],
        hdrs = ["include/mqtt_machine_v0.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@ra//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_mqtt_node_beam",
        testonly = True,
        srcs = ["src/mqtt_node.erl"],
        outs = ["test/mqtt_node.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt.erl"],
        outs = ["test/rabbit_mqtt.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_collector_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt_collector.erl"],
        outs = ["test/rabbit_mqtt_collector.beam"],
        hdrs = ["include/mqtt_machine.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_connection_info_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt_connection_info.erl"],
        outs = ["test/rabbit_mqtt_connection_info.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_connection_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt_connection_sup.erl"],
        outs = ["test/rabbit_mqtt_connection_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@ranch//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_frame_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt_frame.erl"],
        outs = ["test/rabbit_mqtt_frame.beam"],
        hdrs = ["include/rabbit_mqtt_frame.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_internal_event_handler_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt_internal_event_handler.erl"],
        outs = ["test/rabbit_mqtt_internal_event_handler.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_processor_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt_processor.erl"],
        outs = ["test/rabbit_mqtt_processor.beam"],
        hdrs = ["include/rabbit_mqtt.hrl", "include/rabbit_mqtt_frame.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_reader_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt_reader.erl"],
        outs = ["test/rabbit_mqtt_reader.beam"],
        hdrs = ["include/rabbit_mqtt.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_retained_msg_store_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt_retained_msg_store.erl"],
        outs = ["test/rabbit_mqtt_retained_msg_store.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_retained_msg_store_dets_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt_retained_msg_store_dets.erl"],
        outs = ["test/rabbit_mqtt_retained_msg_store_dets.beam"],
        hdrs = ["include/rabbit_mqtt.hrl"],
        beam = ["ebin/rabbit_mqtt_retained_msg_store.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_retained_msg_store_ets_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt_retained_msg_store_ets.erl"],
        outs = ["test/rabbit_mqtt_retained_msg_store_ets.beam"],
        hdrs = ["include/rabbit_mqtt.hrl"],
        beam = ["ebin/rabbit_mqtt_retained_msg_store.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_retained_msg_store_noop_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt_retained_msg_store_noop.erl"],
        outs = ["test/rabbit_mqtt_retained_msg_store_noop.beam"],
        hdrs = ["include/rabbit_mqtt.hrl"],
        beam = ["ebin/rabbit_mqtt_retained_msg_store.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_retainer_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt_retainer.erl"],
        outs = ["test/rabbit_mqtt_retainer.beam"],
        hdrs = ["include/rabbit_mqtt.hrl", "include/rabbit_mqtt_frame.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_retainer_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt_retainer_sup.erl"],
        outs = ["test/rabbit_mqtt_retainer_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt_sup.erl"],
        outs = ["test/rabbit_mqtt_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mqtt_util_beam",
        testonly = True,
        srcs = ["src/rabbit_mqtt_util.erl"],
        outs = ["test/rabbit_mqtt_util.beam"],
        hdrs = ["include/rabbit_mqtt.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/mqtt_machine.hrl", "include/mqtt_machine_v0.hrl", "include/rabbit_mqtt.hrl", "include/rabbit_mqtt_frame.hrl", "include/rabbit_mqtt_retained_msg_store.hrl", "src/Elixir.RabbitMQ.CLI.Ctl.Commands.DecommissionMqttNodeCommand.erl", "src/Elixir.RabbitMQ.CLI.Ctl.Commands.ListMqttConnectionsCommand.erl", "src/mqtt_machine.erl", "src/mqtt_machine_v0.erl", "src/mqtt_node.erl", "src/rabbit_mqtt.erl", "src/rabbit_mqtt_collector.erl", "src/rabbit_mqtt_connection_info.erl", "src/rabbit_mqtt_connection_sup.erl", "src/rabbit_mqtt_frame.erl", "src/rabbit_mqtt_internal_event_handler.erl", "src/rabbit_mqtt_processor.erl", "src/rabbit_mqtt_reader.erl", "src/rabbit_mqtt_retained_msg_store.erl", "src/rabbit_mqtt_retained_msg_store_dets.erl", "src/rabbit_mqtt_retained_msg_store_ets.erl", "src/rabbit_mqtt_retained_msg_store_noop.erl", "src/rabbit_mqtt_retainer.erl", "src/rabbit_mqtt_retainer_sup.erl", "src/rabbit_mqtt_sup.erl", "src/rabbit_mqtt_util.erl"],
    )
