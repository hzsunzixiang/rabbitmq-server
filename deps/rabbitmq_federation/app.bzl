load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.FederationStatusCommand.beam", "ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.RestartFederationLinkCommand.beam", "ebin/rabbit_federation_app.beam", "ebin/rabbit_federation_db.beam", "ebin/rabbit_federation_event.beam", "ebin/rabbit_federation_exchange.beam", "ebin/rabbit_federation_exchange_link.beam", "ebin/rabbit_federation_exchange_link_sup_sup.beam", "ebin/rabbit_federation_link_sup.beam", "ebin/rabbit_federation_link_util.beam", "ebin/rabbit_federation_parameters.beam", "ebin/rabbit_federation_pg.beam", "ebin/rabbit_federation_queue.beam", "ebin/rabbit_federation_queue_link.beam", "ebin/rabbit_federation_queue_link_sup_sup.beam", "ebin/rabbit_federation_status.beam", "ebin/rabbit_federation_sup.beam", "ebin/rabbit_federation_upstream.beam", "ebin/rabbit_federation_upstream_exchange.beam", "ebin/rabbit_federation_util.beam", "ebin/rabbit_log_federation.beam"],
    )
    erlang_bytecode(
        name = "ebin_Elixir_RabbitMQ_CLI_Ctl_Commands_FederationStatusCommand_beam",
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.FederationStatusCommand.erl"],
        outs = ["ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.FederationStatusCommand.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_Elixir_RabbitMQ_CLI_Ctl_Commands_RestartFederationLinkCommand_beam",
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.RestartFederationLinkCommand.erl"],
        outs = ["ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.RestartFederationLinkCommand.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_app_beam",
        srcs = ["src/rabbit_federation_app.erl"],
        outs = ["ebin/rabbit_federation_app.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_db_beam",
        srcs = ["src/rabbit_federation_db.erl"],
        outs = ["ebin/rabbit_federation_db.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_event_beam",
        srcs = ["src/rabbit_federation_event.erl"],
        outs = ["ebin/rabbit_federation_event.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_exchange_beam",
        srcs = ["src/rabbit_federation_exchange.erl"],
        outs = ["ebin/rabbit_federation_exchange.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_exchange_link_beam",
        srcs = ["src/rabbit_federation_exchange_link.erl"],
        outs = ["ebin/rabbit_federation_exchange_link.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_exchange_link_sup_sup_beam",
        srcs = ["src/rabbit_federation_exchange_link_sup_sup.erl"],
        outs = ["ebin/rabbit_federation_exchange_link_sup_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_link_sup_beam",
        srcs = ["src/rabbit_federation_link_sup.erl"],
        outs = ["ebin/rabbit_federation_link_sup.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_link_util_beam",
        srcs = ["src/rabbit_federation_link_util.erl"],
        outs = ["ebin/rabbit_federation_link_util.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_parameters_beam",
        srcs = ["src/rabbit_federation_parameters.erl"],
        outs = ["ebin/rabbit_federation_parameters.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_pg_beam",
        srcs = ["src/rabbit_federation_pg.erl"],
        outs = ["ebin/rabbit_federation_pg.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_queue_beam",
        srcs = ["src/rabbit_federation_queue.erl"],
        outs = ["ebin/rabbit_federation_queue.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_queue_link_beam",
        srcs = ["src/rabbit_federation_queue_link.erl"],
        outs = ["ebin/rabbit_federation_queue_link.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_queue_link_sup_sup_beam",
        srcs = ["src/rabbit_federation_queue_link_sup_sup.erl"],
        outs = ["ebin/rabbit_federation_queue_link_sup_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_status_beam",
        srcs = ["src/rabbit_federation_status.erl"],
        outs = ["ebin/rabbit_federation_status.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_sup_beam",
        srcs = ["src/rabbit_federation_sup.erl"],
        outs = ["ebin/rabbit_federation_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_upstream_beam",
        srcs = ["src/rabbit_federation_upstream.erl"],
        outs = ["ebin/rabbit_federation_upstream.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_upstream_exchange_beam",
        srcs = ["src/rabbit_federation_upstream_exchange.erl"],
        outs = ["ebin/rabbit_federation_upstream_exchange.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_federation_util_beam",
        srcs = ["src/rabbit_federation_util.erl"],
        outs = ["ebin/rabbit_federation_util.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_log_federation_beam",
        srcs = ["src/rabbit_log_federation.erl"],
        outs = ["ebin/rabbit_log_federation.beam"],
        hdrs = ["include/logging.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_test_beam_files():
    erlang_bytecode(
        name = "test_Elixir_RabbitMQ_CLI_Ctl_Commands_FederationStatusCommand_beam",
        testonly = True,
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.FederationStatusCommand.erl"],
        outs = ["test/Elixir.RabbitMQ.CLI.Ctl.Commands.FederationStatusCommand.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    erlang_bytecode(
        name = "test_Elixir_RabbitMQ_CLI_Ctl_Commands_RestartFederationLinkCommand_beam",
        testonly = True,
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.RestartFederationLinkCommand.erl"],
        outs = ["test/Elixir.RabbitMQ.CLI.Ctl.Commands.RestartFederationLinkCommand.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/Elixir.RabbitMQ.CLI.Ctl.Commands.FederationStatusCommand.beam", "test/Elixir.RabbitMQ.CLI.Ctl.Commands.RestartFederationLinkCommand.beam", "test/rabbit_federation_app.beam", "test/rabbit_federation_db.beam", "test/rabbit_federation_event.beam", "test/rabbit_federation_exchange.beam", "test/rabbit_federation_exchange_link.beam", "test/rabbit_federation_exchange_link_sup_sup.beam", "test/rabbit_federation_link_sup.beam", "test/rabbit_federation_link_util.beam", "test/rabbit_federation_parameters.beam", "test/rabbit_federation_pg.beam", "test/rabbit_federation_queue.beam", "test/rabbit_federation_queue_link.beam", "test/rabbit_federation_queue_link_sup_sup.beam", "test/rabbit_federation_status.beam", "test/rabbit_federation_sup.beam", "test/rabbit_federation_upstream.beam", "test/rabbit_federation_upstream_exchange.beam", "test/rabbit_federation_util.beam", "test/rabbit_log_federation.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_app_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_app.erl"],
        outs = ["test/rabbit_federation_app.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_federation_db_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_db.erl"],
        outs = ["test/rabbit_federation_db.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_event_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_event.erl"],
        outs = ["test/rabbit_federation_event.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_exchange_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_exchange.erl"],
        outs = ["test/rabbit_federation_exchange.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_exchange_link_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_exchange_link.erl"],
        outs = ["test/rabbit_federation_exchange_link.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_exchange_link_sup_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_exchange_link_sup_sup.erl"],
        outs = ["test/rabbit_federation_exchange_link_sup_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_link_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_link_sup.erl"],
        outs = ["test/rabbit_federation_link_sup.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_link_util_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_link_util.erl"],
        outs = ["test/rabbit_federation_link_util.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_parameters_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_parameters.erl"],
        outs = ["test/rabbit_federation_parameters.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_pg_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_pg.erl"],
        outs = ["test/rabbit_federation_pg.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_federation_queue_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_queue.erl"],
        outs = ["test/rabbit_federation_queue.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_queue_link_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_queue_link.erl"],
        outs = ["test/rabbit_federation_queue_link.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_queue_link_sup_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_queue_link_sup_sup.erl"],
        outs = ["test/rabbit_federation_queue_link_sup_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_status_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_status.erl"],
        outs = ["test/rabbit_federation_status.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_sup.erl"],
        outs = ["test/rabbit_federation_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_upstream_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_upstream.erl"],
        outs = ["test/rabbit_federation_upstream.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_upstream_exchange_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_upstream_exchange.erl"],
        outs = ["test/rabbit_federation_upstream_exchange.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_federation_util_beam",
        testonly = True,
        srcs = ["src/rabbit_federation_util.erl"],
        outs = ["test/rabbit_federation_util.beam"],
        hdrs = ["include/rabbit_federation.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_log_federation_beam",
        testonly = True,
        srcs = ["src/rabbit_log_federation.erl"],
        outs = ["test/rabbit_log_federation.beam"],
        hdrs = ["include/logging.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/logging.hrl", "include/rabbit_federation.hrl", "src/Elixir.RabbitMQ.CLI.Ctl.Commands.FederationStatusCommand.erl", "src/Elixir.RabbitMQ.CLI.Ctl.Commands.RestartFederationLinkCommand.erl", "src/rabbit_federation_app.erl", "src/rabbit_federation_db.erl", "src/rabbit_federation_event.erl", "src/rabbit_federation_exchange.erl", "src/rabbit_federation_exchange_link.erl", "src/rabbit_federation_exchange_link_sup_sup.erl", "src/rabbit_federation_link_sup.erl", "src/rabbit_federation_link_util.erl", "src/rabbit_federation_parameters.erl", "src/rabbit_federation_pg.erl", "src/rabbit_federation_queue.erl", "src/rabbit_federation_queue_link.erl", "src/rabbit_federation_queue_link_sup_sup.erl", "src/rabbit_federation_status.erl", "src/rabbit_federation_sup.erl", "src/rabbit_federation_upstream.erl", "src/rabbit_federation_upstream_exchange.erl", "src/rabbit_federation_util.erl", "src/rabbit_log_federation.erl"],
    )
