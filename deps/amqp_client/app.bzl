load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/amqp_auth_mechanisms.beam", "ebin/amqp_channel.beam", "ebin/amqp_channel_sup.beam", "ebin/amqp_channel_sup_sup.beam", "ebin/amqp_channels_manager.beam", "ebin/amqp_client.beam", "ebin/amqp_connection.beam", "ebin/amqp_connection_sup.beam", "ebin/amqp_connection_type_sup.beam", "ebin/amqp_direct_connection.beam", "ebin/amqp_direct_consumer.beam", "ebin/amqp_gen_connection.beam", "ebin/amqp_gen_consumer.beam", "ebin/amqp_main_reader.beam", "ebin/amqp_network_connection.beam", "ebin/amqp_rpc_client.beam", "ebin/amqp_rpc_server.beam", "ebin/amqp_selective_consumer.beam", "ebin/amqp_ssl.beam", "ebin/amqp_sup.beam", "ebin/amqp_uri.beam", "ebin/amqp_util.beam", "ebin/rabbit_routing_util.beam", "ebin/uri_parser.beam"],
    )
    erlang_bytecode(
        name = "ebin_amqp_auth_mechanisms_beam",
        srcs = ["src/amqp_auth_mechanisms.erl"],
        outs = ["ebin/amqp_auth_mechanisms.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_channel_beam",
        srcs = ["src/amqp_channel.erl"],
        outs = ["ebin/amqp_channel.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_channel_sup_beam",
        srcs = ["src/amqp_channel_sup.erl"],
        outs = ["ebin/amqp_channel_sup.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_channel_sup_sup_beam",
        srcs = ["src/amqp_channel_sup_sup.erl"],
        outs = ["ebin/amqp_channel_sup_sup.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_channels_manager_beam",
        srcs = ["src/amqp_channels_manager.erl"],
        outs = ["ebin/amqp_channels_manager.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_client_beam",
        srcs = ["src/amqp_client.erl"],
        outs = ["ebin/amqp_client.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_amqp_connection_beam",
        srcs = ["src/amqp_connection.erl"],
        outs = ["ebin/amqp_connection.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_connection_sup_beam",
        srcs = ["src/amqp_connection_sup.erl"],
        outs = ["ebin/amqp_connection_sup.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_connection_type_sup_beam",
        srcs = ["src/amqp_connection_type_sup.erl"],
        outs = ["ebin/amqp_connection_type_sup.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_direct_connection_beam",
        srcs = ["src/amqp_direct_connection.erl"],
        outs = ["ebin/amqp_direct_connection.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        beam = ["ebin/amqp_gen_connection.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_direct_consumer_beam",
        srcs = ["src/amqp_direct_consumer.erl"],
        outs = ["ebin/amqp_direct_consumer.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_gen_consumer_spec.hrl"],
        beam = ["ebin/amqp_gen_consumer.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_gen_connection_beam",
        srcs = ["src/amqp_gen_connection.erl"],
        outs = ["ebin/amqp_gen_connection.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_gen_consumer_beam",
        srcs = ["src/amqp_gen_consumer.erl"],
        outs = ["ebin/amqp_gen_consumer.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_main_reader_beam",
        srcs = ["src/amqp_main_reader.erl"],
        outs = ["ebin/amqp_main_reader.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_network_connection_beam",
        srcs = ["src/amqp_network_connection.erl"],
        outs = ["ebin/amqp_network_connection.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        beam = ["ebin/amqp_gen_connection.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_rpc_client_beam",
        srcs = ["src/amqp_rpc_client.erl"],
        outs = ["ebin/amqp_rpc_client.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_rpc_server_beam",
        srcs = ["src/amqp_rpc_server.erl"],
        outs = ["ebin/amqp_rpc_server.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_selective_consumer_beam",
        srcs = ["src/amqp_selective_consumer.erl"],
        outs = ["ebin/amqp_selective_consumer.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_gen_consumer_spec.hrl"],
        beam = ["ebin/amqp_gen_consumer.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_ssl_beam",
        srcs = ["src/amqp_ssl.erl"],
        outs = ["ebin/amqp_ssl.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_sup_beam",
        srcs = ["src/amqp_sup.erl"],
        outs = ["ebin/amqp_sup.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_uri_beam",
        srcs = ["src/amqp_uri.erl"],
        outs = ["ebin/amqp_uri.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_amqp_util_beam",
        srcs = ["src/amqp_util.erl"],
        outs = ["ebin/amqp_util.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_routing_util_beam",
        srcs = ["src/rabbit_routing_util.erl"],
        outs = ["ebin/rabbit_routing_util.beam"],
        hdrs = ["include/amqp_client.hrl", "include/rabbit_routing_prefixes.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_uri_parser_beam",
        srcs = ["src/uri_parser.erl"],
        outs = ["ebin/uri_parser.beam"],
        erlc_opts = "//:erlc_opts",
    )

def all_test_beam_files():
    erlang_bytecode(
        name = "test_amqp_auth_mechanisms_beam",
        testonly = True,
        srcs = ["src/amqp_auth_mechanisms.erl"],
        outs = ["test/amqp_auth_mechanisms.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_channel_beam",
        testonly = True,
        srcs = ["src/amqp_channel.erl"],
        outs = ["test/amqp_channel.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_channel_sup_beam",
        testonly = True,
        srcs = ["src/amqp_channel_sup.erl"],
        outs = ["test/amqp_channel_sup.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_channel_sup_sup_beam",
        testonly = True,
        srcs = ["src/amqp_channel_sup_sup.erl"],
        outs = ["test/amqp_channel_sup_sup.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_channels_manager_beam",
        testonly = True,
        srcs = ["src/amqp_channels_manager.erl"],
        outs = ["test/amqp_channels_manager.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_client_beam",
        testonly = True,
        srcs = ["src/amqp_client.erl"],
        outs = ["test/amqp_client.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_amqp_connection_beam",
        testonly = True,
        srcs = ["src/amqp_connection.erl"],
        outs = ["test/amqp_connection.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_connection_sup_beam",
        testonly = True,
        srcs = ["src/amqp_connection_sup.erl"],
        outs = ["test/amqp_connection_sup.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_connection_type_sup_beam",
        testonly = True,
        srcs = ["src/amqp_connection_type_sup.erl"],
        outs = ["test/amqp_connection_type_sup.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_direct_connection_beam",
        testonly = True,
        srcs = ["src/amqp_direct_connection.erl"],
        outs = ["test/amqp_direct_connection.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        beam = ["ebin/amqp_gen_connection.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_direct_consumer_beam",
        testonly = True,
        srcs = ["src/amqp_direct_consumer.erl"],
        outs = ["test/amqp_direct_consumer.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_gen_consumer_spec.hrl"],
        beam = ["ebin/amqp_gen_consumer.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_gen_connection_beam",
        testonly = True,
        srcs = ["src/amqp_gen_connection.erl"],
        outs = ["test/amqp_gen_connection.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_gen_consumer_beam",
        testonly = True,
        srcs = ["src/amqp_gen_consumer.erl"],
        outs = ["test/amqp_gen_consumer.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_main_reader_beam",
        testonly = True,
        srcs = ["src/amqp_main_reader.erl"],
        outs = ["test/amqp_main_reader.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_network_connection_beam",
        testonly = True,
        srcs = ["src/amqp_network_connection.erl"],
        outs = ["test/amqp_network_connection.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        beam = ["ebin/amqp_gen_connection.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_rpc_client_beam",
        testonly = True,
        srcs = ["src/amqp_rpc_client.erl"],
        outs = ["test/amqp_rpc_client.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_rpc_server_beam",
        testonly = True,
        srcs = ["src/amqp_rpc_server.erl"],
        outs = ["test/amqp_rpc_server.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_selective_consumer_beam",
        testonly = True,
        srcs = ["src/amqp_selective_consumer.erl"],
        outs = ["test/amqp_selective_consumer.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_gen_consumer_spec.hrl"],
        beam = ["ebin/amqp_gen_consumer.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_ssl_beam",
        testonly = True,
        srcs = ["src/amqp_ssl.erl"],
        outs = ["test/amqp_ssl.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_sup_beam",
        testonly = True,
        srcs = ["src/amqp_sup.erl"],
        outs = ["test/amqp_sup.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_uri_beam",
        testonly = True,
        srcs = ["src/amqp_uri.erl"],
        outs = ["test/amqp_uri.beam"],
        hdrs = ["include/amqp_client.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_amqp_util_beam",
        testonly = True,
        srcs = ["src/amqp_util.erl"],
        outs = ["test/amqp_util.beam"],
        hdrs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/amqp_auth_mechanisms.beam", "test/amqp_channel.beam", "test/amqp_channel_sup.beam", "test/amqp_channel_sup_sup.beam", "test/amqp_channels_manager.beam", "test/amqp_client.beam", "test/amqp_connection.beam", "test/amqp_connection_sup.beam", "test/amqp_connection_type_sup.beam", "test/amqp_direct_connection.beam", "test/amqp_direct_consumer.beam", "test/amqp_gen_connection.beam", "test/amqp_gen_consumer.beam", "test/amqp_main_reader.beam", "test/amqp_network_connection.beam", "test/amqp_rpc_client.beam", "test/amqp_rpc_server.beam", "test/amqp_selective_consumer.beam", "test/amqp_ssl.beam", "test/amqp_sup.beam", "test/amqp_uri.beam", "test/amqp_util.beam", "test/rabbit_routing_util.beam", "test/uri_parser.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_routing_util_beam",
        testonly = True,
        srcs = ["src/rabbit_routing_util.erl"],
        outs = ["test/rabbit_routing_util.beam"],
        hdrs = ["include/amqp_client.hrl", "include/rabbit_routing_prefixes.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_uri_parser_beam",
        testonly = True,
        srcs = ["src/uri_parser.erl"],
        outs = ["test/uri_parser.beam"],
        erlc_opts = "//:test_erlc_opts",
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/amqp_client.hrl", "include/amqp_client_internal.hrl", "include/amqp_gen_consumer_spec.hrl", "include/rabbit_routing_prefixes.hrl", "src/amqp_auth_mechanisms.erl", "src/amqp_channel.erl", "src/amqp_channel_sup.erl", "src/amqp_channel_sup_sup.erl", "src/amqp_channels_manager.erl", "src/amqp_client.erl", "src/amqp_connection.erl", "src/amqp_connection_sup.erl", "src/amqp_connection_type_sup.erl", "src/amqp_direct_connection.erl", "src/amqp_direct_consumer.erl", "src/amqp_gen_connection.erl", "src/amqp_gen_consumer.erl", "src/amqp_main_reader.erl", "src/amqp_network_connection.erl", "src/amqp_rpc_client.erl", "src/amqp_rpc_server.erl", "src/amqp_selective_consumer.erl", "src/amqp_ssl.erl", "src/amqp_sup.erl", "src/amqp_uri.erl", "src/amqp_util.erl", "src/rabbit_routing_util.erl", "src/uri_parser.erl"],
    )
