load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.AddUaaKeyCommand.beam", "ebin/rabbit_auth_backend_oauth2.beam", "ebin/rabbit_auth_backend_oauth2_app.beam", "ebin/rabbit_oauth2_scope.beam", "ebin/uaa_jwks.beam", "ebin/uaa_jwt.beam", "ebin/uaa_jwt_jwk.beam", "ebin/uaa_jwt_jwt.beam", "ebin/wildcard.beam"],
    )
    erlang_bytecode(
        name = "ebin_Elixir_RabbitMQ_CLI_Ctl_Commands_AddUaaKeyCommand_beam",
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.AddUaaKeyCommand.erl"],
        outs = ["ebin/Elixir.RabbitMQ.CLI.Ctl.Commands.AddUaaKeyCommand.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_backend_oauth2_app_beam",
        srcs = ["src/rabbit_auth_backend_oauth2_app.erl"],
        outs = ["ebin/rabbit_auth_backend_oauth2_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_backend_oauth2_beam",
        srcs = ["src/rabbit_auth_backend_oauth2.erl"],
        outs = ["ebin/rabbit_auth_backend_oauth2.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_oauth2_scope_beam",
        srcs = ["src/rabbit_oauth2_scope.erl"],
        outs = ["ebin/rabbit_oauth2_scope.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_uaa_jwks_beam",
        srcs = ["src/uaa_jwks.erl"],
        outs = ["ebin/uaa_jwks.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_uaa_jwt_beam",
        srcs = ["src/uaa_jwt.erl"],
        outs = ["ebin/uaa_jwt.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["@jose//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_uaa_jwt_jwk_beam",
        srcs = ["src/uaa_jwt_jwk.erl"],
        outs = ["ebin/uaa_jwt_jwk.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["@jose//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_uaa_jwt_jwt_beam",
        srcs = ["src/uaa_jwt_jwt.erl"],
        outs = ["ebin/uaa_jwt_jwt.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["@jose//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_wildcard_beam",
        srcs = ["src/wildcard.erl"],
        outs = ["ebin/wildcard.beam"],
        erlc_opts = "//:erlc_opts",
    )

def all_test_beam_files():
    erlang_bytecode(
        name = "test_Elixir_RabbitMQ_CLI_Ctl_Commands_AddUaaKeyCommand_beam",
        testonly = True,
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.AddUaaKeyCommand.erl"],
        outs = ["test/Elixir.RabbitMQ.CLI.Ctl.Commands.AddUaaKeyCommand.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_cli:erlang_app"],
    )
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/Elixir.RabbitMQ.CLI.Ctl.Commands.AddUaaKeyCommand.beam", "test/rabbit_auth_backend_oauth2.beam", "test/rabbit_auth_backend_oauth2_app.beam", "test/rabbit_oauth2_scope.beam", "test/uaa_jwks.beam", "test/uaa_jwt.beam", "test/uaa_jwt_jwk.beam", "test/uaa_jwt_jwt.beam", "test/wildcard.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_auth_backend_oauth2_app_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_backend_oauth2_app.erl"],
        outs = ["test/rabbit_auth_backend_oauth2_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_auth_backend_oauth2_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_backend_oauth2.erl"],
        outs = ["test/rabbit_auth_backend_oauth2.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_oauth2_scope_beam",
        testonly = True,
        srcs = ["src/rabbit_oauth2_scope.erl"],
        outs = ["test/rabbit_oauth2_scope.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_uaa_jwks_beam",
        testonly = True,
        srcs = ["src/uaa_jwks.erl"],
        outs = ["test/uaa_jwks.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_uaa_jwt_beam",
        testonly = True,
        srcs = ["src/uaa_jwt.erl"],
        outs = ["test/uaa_jwt.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@jose//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_uaa_jwt_jwk_beam",
        testonly = True,
        srcs = ["src/uaa_jwt_jwk.erl"],
        outs = ["test/uaa_jwt_jwk.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@jose//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_uaa_jwt_jwt_beam",
        testonly = True,
        srcs = ["src/uaa_jwt_jwt.erl"],
        outs = ["test/uaa_jwt_jwt.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@jose//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_wildcard_beam",
        testonly = True,
        srcs = ["src/wildcard.erl"],
        outs = ["test/wildcard.beam"],
        erlc_opts = "//:test_erlc_opts",
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/Elixir.RabbitMQ.CLI.Ctl.Commands.AddUaaKeyCommand.erl", "src/rabbit_auth_backend_oauth2.erl", "src/rabbit_auth_backend_oauth2_app.erl", "src/rabbit_oauth2_scope.erl", "src/uaa_jwks.erl", "src/uaa_jwt.erl", "src/uaa_jwt_jwk.erl", "src/uaa_jwt_jwt.erl", "src/wildcard.erl"],
    )
