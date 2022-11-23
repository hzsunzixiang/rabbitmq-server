load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.DeleteShovelCommand.beam", "ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.RestartShovelCommand.beam", "ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.ShovelStatusCommand.beam", "ebin/rabbit_amqp091_shovel.beam", "ebin/rabbit_amqp10_shovel.beam", "ebin/rabbit_log_shovel.beam", "ebin/rabbit_shovel.beam", "ebin/rabbit_shovel_behaviour.beam", "ebin/rabbit_shovel_config.beam", "ebin/rabbit_shovel_dyn_worker_sup.beam", "ebin/rabbit_shovel_dyn_worker_sup_sup.beam", "ebin/rabbit_shovel_locks.beam", "ebin/rabbit_shovel_parameters.beam", "ebin/rabbit_shovel_status.beam", "ebin/rabbit_shovel_sup.beam", "ebin/rabbit_shovel_util.beam", "ebin/rabbit_shovel_worker.beam", "ebin/rabbit_shovel_worker_sup.beam"],
    )
    erlang_bytecode(
        name = "ebin_Elixir_RabbitMQ_CLI_Ctl_Commands_DeleteShovelCommand_beam",
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.DeleteShovelCommand.erl"],
        outs = ["ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.DeleteShovelCommand.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_Elixir_RabbitMQ_CLI_Ctl_Commands_RestartShovelCommand_beam",
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.RestartShovelCommand.erl"],
        outs = ["ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.RestartShovelCommand.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_Elixir_RabbitMQ_CLI_Ctl_Commands_ShovelStatusCommand_beam",
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.ShovelStatusCommand.erl"],
        outs = ["ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.ShovelStatusCommand.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp091_shovel_beam",
        srcs = ["src/rabbit_amqp091_shovel.erl"],
        outs = ["ebin/rabbit_amqp091_shovel.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        beam = ["ebin/rabbit_shovel_behaviour.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp10_shovel_beam",
        srcs = ["src/rabbit_amqp10_shovel.erl"],
        outs = ["ebin/rabbit_amqp10_shovel.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        beam = ["ebin/rabbit_shovel_behaviour.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_log_shovel_beam",
        srcs = ["src/rabbit_log_shovel.erl"],
        outs = ["ebin/rabbit_log_shovel.beam"],
        hdrs = ["include/logging.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_shovel_beam",
        srcs = ["src/rabbit_shovel.erl"],
        outs = ["ebin/rabbit_shovel.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_shovel_behaviour_beam",
        srcs = ["src/rabbit_shovel_behaviour.erl"],
        outs = ["ebin/rabbit_shovel_behaviour.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_shovel_config_beam",
        srcs = ["src/rabbit_shovel_config.erl"],
        outs = ["ebin/rabbit_shovel_config.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_shovel_dyn_worker_sup_beam",
        srcs = ["src/rabbit_shovel_dyn_worker_sup.erl"],
        outs = ["ebin/rabbit_shovel_dyn_worker_sup.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_shovel_dyn_worker_sup_sup_beam",
        srcs = ["src/rabbit_shovel_dyn_worker_sup_sup.erl"],
        outs = ["ebin/rabbit_shovel_dyn_worker_sup_sup.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_shovel_locks_beam",
        srcs = ["src/rabbit_shovel_locks.erl"],
        outs = ["ebin/rabbit_shovel_locks.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_shovel_parameters_beam",
        srcs = ["src/rabbit_shovel_parameters.erl"],
        outs = ["ebin/rabbit_shovel_parameters.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_shovel_status_beam",
        srcs = ["src/rabbit_shovel_status.erl"],
        outs = ["ebin/rabbit_shovel_status.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_shovel_sup_beam",
        srcs = ["src/rabbit_shovel_sup.erl"],
        outs = ["ebin/rabbit_shovel_sup.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_shovel_util_beam",
        srcs = ["src/rabbit_shovel_util.erl"],
        outs = ["ebin/rabbit_shovel_util.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_shovel_worker_beam",
        srcs = ["src/rabbit_shovel_worker.erl"],
        outs = ["ebin/rabbit_shovel_worker.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_shovel_worker_sup_beam",
        srcs = ["src/rabbit_shovel_worker_sup.erl"],
        outs = ["ebin/rabbit_shovel_worker_sup.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )

def all_test_beam_files():
    erlang_bytecode(
        name = "test_Elixir_RabbitMQ_CLI_Ctl_Commands_DeleteShovelCommand_beam",
        testonly = True,
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.DeleteShovelCommand.erl"],
        outs = ["test/Elixir.RabbitMQ.CLI.Ctl.Commands.DeleteShovelCommand.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    erlang_bytecode(
        name = "test_Elixir_RabbitMQ_CLI_Ctl_Commands_RestartShovelCommand_beam",
        testonly = True,
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.RestartShovelCommand.erl"],
        outs = ["test/Elixir.RabbitMQ.CLI.Ctl.Commands.RestartShovelCommand.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    erlang_bytecode(
        name = "test_Elixir_RabbitMQ_CLI_Ctl_Commands_ShovelStatusCommand_beam",
        testonly = True,
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.ShovelStatusCommand.erl"],
        outs = ["test/Elixir.RabbitMQ.CLI.Ctl.Commands.ShovelStatusCommand.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/Elixir.RabbitMQ.CLI.Ctl.Commands.DeleteShovelCommand.beam", "test/Elixir.RabbitMQ.CLI.Ctl.Commands.RestartShovelCommand.beam", "test/Elixir.RabbitMQ.CLI.Ctl.Commands.ShovelStatusCommand.beam", "test/rabbit_amqp091_shovel.beam", "test/rabbit_amqp10_shovel.beam", "test/rabbit_log_shovel.beam", "test/rabbit_shovel.beam", "test/rabbit_shovel_behaviour.beam", "test/rabbit_shovel_config.beam", "test/rabbit_shovel_dyn_worker_sup.beam", "test/rabbit_shovel_dyn_worker_sup_sup.beam", "test/rabbit_shovel_locks.beam", "test/rabbit_shovel_parameters.beam", "test/rabbit_shovel_status.beam", "test/rabbit_shovel_sup.beam", "test/rabbit_shovel_util.beam", "test/rabbit_shovel_worker.beam", "test/rabbit_shovel_worker_sup.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqp091_shovel_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp091_shovel.erl"],
        outs = ["test/rabbit_amqp091_shovel.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        beam = ["ebin/rabbit_shovel_behaviour.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqp10_shovel_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp10_shovel.erl"],
        outs = ["test/rabbit_amqp10_shovel.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        beam = ["ebin/rabbit_shovel_behaviour.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_log_shovel_beam",
        testonly = True,
        srcs = ["src/rabbit_log_shovel.erl"],
        outs = ["test/rabbit_log_shovel.beam"],
        hdrs = ["include/logging.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_shovel_beam",
        testonly = True,
        srcs = ["src/rabbit_shovel.erl"],
        outs = ["test/rabbit_shovel.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_shovel_behaviour_beam",
        testonly = True,
        srcs = ["src/rabbit_shovel_behaviour.erl"],
        outs = ["test/rabbit_shovel_behaviour.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_shovel_config_beam",
        testonly = True,
        srcs = ["src/rabbit_shovel_config.erl"],
        outs = ["test/rabbit_shovel_config.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_shovel_dyn_worker_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_shovel_dyn_worker_sup.erl"],
        outs = ["test/rabbit_shovel_dyn_worker_sup.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_shovel_dyn_worker_sup_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_shovel_dyn_worker_sup_sup.erl"],
        outs = ["test/rabbit_shovel_dyn_worker_sup_sup.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_shovel_locks_beam",
        testonly = True,
        srcs = ["src/rabbit_shovel_locks.erl"],
        outs = ["test/rabbit_shovel_locks.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_shovel_parameters_beam",
        testonly = True,
        srcs = ["src/rabbit_shovel_parameters.erl"],
        outs = ["test/rabbit_shovel_parameters.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_shovel_status_beam",
        testonly = True,
        srcs = ["src/rabbit_shovel_status.erl"],
        outs = ["test/rabbit_shovel_status.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_shovel_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_shovel_sup.erl"],
        outs = ["test/rabbit_shovel_sup.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_shovel_util_beam",
        testonly = True,
        srcs = ["src/rabbit_shovel_util.erl"],
        outs = ["test/rabbit_shovel_util.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_shovel_worker_beam",
        testonly = True,
        srcs = ["src/rabbit_shovel_worker.erl"],
        outs = ["test/rabbit_shovel_worker.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_shovel_worker_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_shovel_worker_sup.erl"],
        outs = ["test/rabbit_shovel_worker_sup.beam"],
        hdrs = ["include/rabbit_shovel.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/logging.hrl", "include/rabbit_shovel.hrl", "src/Elixir.RabbitMQ.CLI.Ctl.Commands.DeleteShovelCommand.erl", "src/Elixir.RabbitMQ.CLI.Ctl.Commands.RestartShovelCommand.erl", "src/Elixir.RabbitMQ.CLI.Ctl.Commands.ShovelStatusCommand.erl", "src/rabbit_amqp091_shovel.erl", "src/rabbit_amqp10_shovel.erl", "src/rabbit_log_shovel.erl", "src/rabbit_shovel.erl", "src/rabbit_shovel_behaviour.erl", "src/rabbit_shovel_config.erl", "src/rabbit_shovel_dyn_worker_sup.erl", "src/rabbit_shovel_dyn_worker_sup_sup.erl", "src/rabbit_shovel_locks.erl", "src/rabbit_shovel_parameters.erl", "src/rabbit_shovel_status.erl", "src/rabbit_shovel_sup.erl", "src/rabbit_shovel_util.erl", "src/rabbit_shovel_worker.erl", "src/rabbit_shovel_worker_sup.erl"],
    )
