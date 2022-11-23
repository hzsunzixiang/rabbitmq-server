load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/Elixir.RabbitMQ.CLI.Diagnostics.Commands.ConsistentHashExchangeRingStateCommand.beam", "ebin/rabbit_exchange_type_consistent_hash.beam"],
    )
    erlang_bytecode(
        name = "ebin_Elixir_RabbitMQ_CLI_Diagnostics_Commands_ConsistentHashExchangeRingStateCommand_beam",
        srcs = ["src/Elixir.RabbitMQ.CLI.Diagnostics.Commands.ConsistentHashExchangeRingStateCommand.erl"],
        outs = ["ebin/Elixir.RabbitMQ.CLI.Diagnostics.Commands.ConsistentHashExchangeRingStateCommand.beam"],
        hdrs = ["include/rabbitmq_consistent_hash_exchange.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_cli:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_exchange_type_consistent_hash_beam",
        srcs = ["src/rabbit_exchange_type_consistent_hash.erl"],
        outs = ["ebin/rabbit_exchange_type_consistent_hash.beam"],
        hdrs = ["include/rabbitmq_consistent_hash_exchange.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_test_beam_files():
    erlang_bytecode(
        name = "test_Elixir_RabbitMQ_CLI_Diagnostics_Commands_ConsistentHashExchangeRingStateCommand_beam",
        testonly = True,
        srcs = ["src/Elixir.RabbitMQ.CLI.Diagnostics.Commands.ConsistentHashExchangeRingStateCommand.erl"],
        outs = ["test/Elixir.RabbitMQ.CLI.Diagnostics.Commands.ConsistentHashExchangeRingStateCommand.beam"],
        hdrs = ["include/rabbitmq_consistent_hash_exchange.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_cli:erlang_app"],
    )
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/Elixir.RabbitMQ.CLI.Diagnostics.Commands.ConsistentHashExchangeRingStateCommand.beam", "test/rabbit_exchange_type_consistent_hash.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_exchange_type_consistent_hash_beam",
        testonly = True,
        srcs = ["src/rabbit_exchange_type_consistent_hash.erl"],
        outs = ["test/rabbit_exchange_type_consistent_hash.beam"],
        hdrs = ["include/rabbitmq_consistent_hash_exchange.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/rabbitmq_consistent_hash_exchange.hrl", "src/Elixir.RabbitMQ.CLI.Diagnostics.Commands.ConsistentHashExchangeRingStateCommand.erl", "src/rabbit_exchange_type_consistent_hash.erl"],
    )
