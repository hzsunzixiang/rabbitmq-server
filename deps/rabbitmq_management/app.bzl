load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_mgmt_app.beam", "ebin/rabbit_mgmt_cors.beam", "ebin/rabbit_mgmt_csp.beam", "ebin/rabbit_mgmt_db.beam", "ebin/rabbit_mgmt_db_cache.beam", "ebin/rabbit_mgmt_db_cache_sup.beam", "ebin/rabbit_mgmt_dispatcher.beam", "ebin/rabbit_mgmt_extension.beam", "ebin/rabbit_mgmt_headers.beam", "ebin/rabbit_mgmt_hsts.beam", "ebin/rabbit_mgmt_load_definitions.beam", "ebin/rabbit_mgmt_reset_handler.beam", "ebin/rabbit_mgmt_stats.beam", "ebin/rabbit_mgmt_sup.beam", "ebin/rabbit_mgmt_sup_sup.beam", "ebin/rabbit_mgmt_util.beam", "ebin/rabbit_mgmt_wm_aliveness_test.beam", "ebin/rabbit_mgmt_wm_auth.beam", "ebin/rabbit_mgmt_wm_auth_attempts.beam", "ebin/rabbit_mgmt_wm_binding.beam", "ebin/rabbit_mgmt_wm_bindings.beam", "ebin/rabbit_mgmt_wm_channel.beam", "ebin/rabbit_mgmt_wm_channels.beam", "ebin/rabbit_mgmt_wm_channels_vhost.beam", "ebin/rabbit_mgmt_wm_cluster_name.beam", "ebin/rabbit_mgmt_wm_connection.beam", "ebin/rabbit_mgmt_wm_connection_channels.beam", "ebin/rabbit_mgmt_wm_connection_user_name.beam", "ebin/rabbit_mgmt_wm_connections.beam", "ebin/rabbit_mgmt_wm_connections_vhost.beam", "ebin/rabbit_mgmt_wm_consumers.beam", "ebin/rabbit_mgmt_wm_definitions.beam", "ebin/rabbit_mgmt_wm_exchange.beam", "ebin/rabbit_mgmt_wm_exchange_publish.beam", "ebin/rabbit_mgmt_wm_exchanges.beam", "ebin/rabbit_mgmt_wm_extensions.beam", "ebin/rabbit_mgmt_wm_feature_flag_enable.beam", "ebin/rabbit_mgmt_wm_feature_flags.beam", "ebin/rabbit_mgmt_wm_global_parameter.beam", "ebin/rabbit_mgmt_wm_global_parameters.beam", "ebin/rabbit_mgmt_wm_health_check_alarms.beam", "ebin/rabbit_mgmt_wm_health_check_certificate_expiration.beam", "ebin/rabbit_mgmt_wm_health_check_local_alarms.beam", "ebin/rabbit_mgmt_wm_health_check_node_is_mirror_sync_critical.beam", "ebin/rabbit_mgmt_wm_health_check_node_is_quorum_critical.beam", "ebin/rabbit_mgmt_wm_health_check_port_listener.beam", "ebin/rabbit_mgmt_wm_health_check_protocol_listener.beam", "ebin/rabbit_mgmt_wm_health_check_virtual_hosts.beam", "ebin/rabbit_mgmt_wm_healthchecks.beam", "ebin/rabbit_mgmt_wm_limit.beam", "ebin/rabbit_mgmt_wm_limits.beam", "ebin/rabbit_mgmt_wm_login.beam", "ebin/rabbit_mgmt_wm_node.beam", "ebin/rabbit_mgmt_wm_node_memory.beam", "ebin/rabbit_mgmt_wm_node_memory_ets.beam", "ebin/rabbit_mgmt_wm_nodes.beam", "ebin/rabbit_mgmt_wm_operator_policies.beam", "ebin/rabbit_mgmt_wm_operator_policy.beam", "ebin/rabbit_mgmt_wm_overview.beam", "ebin/rabbit_mgmt_wm_parameter.beam", "ebin/rabbit_mgmt_wm_parameters.beam", "ebin/rabbit_mgmt_wm_permission.beam", "ebin/rabbit_mgmt_wm_permissions.beam", "ebin/rabbit_mgmt_wm_permissions_user.beam", "ebin/rabbit_mgmt_wm_permissions_vhost.beam", "ebin/rabbit_mgmt_wm_policies.beam", "ebin/rabbit_mgmt_wm_policy.beam", "ebin/rabbit_mgmt_wm_queue.beam", "ebin/rabbit_mgmt_wm_queue_actions.beam", "ebin/rabbit_mgmt_wm_queue_get.beam", "ebin/rabbit_mgmt_wm_queue_purge.beam", "ebin/rabbit_mgmt_wm_queues.beam", "ebin/rabbit_mgmt_wm_rebalance_queues.beam", "ebin/rabbit_mgmt_wm_redirect.beam", "ebin/rabbit_mgmt_wm_reset.beam", "ebin/rabbit_mgmt_wm_static.beam", "ebin/rabbit_mgmt_wm_topic_permission.beam", "ebin/rabbit_mgmt_wm_topic_permissions.beam", "ebin/rabbit_mgmt_wm_topic_permissions_user.beam", "ebin/rabbit_mgmt_wm_topic_permissions_vhost.beam", "ebin/rabbit_mgmt_wm_user.beam", "ebin/rabbit_mgmt_wm_user_limit.beam", "ebin/rabbit_mgmt_wm_user_limits.beam", "ebin/rabbit_mgmt_wm_users.beam", "ebin/rabbit_mgmt_wm_users_bulk_delete.beam", "ebin/rabbit_mgmt_wm_vhost.beam", "ebin/rabbit_mgmt_wm_vhost_restart.beam", "ebin/rabbit_mgmt_wm_vhosts.beam", "ebin/rabbit_mgmt_wm_whoami.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_app_beam",
        srcs = ["src/rabbit_mgmt_app.erl"],
        outs = ["ebin/rabbit_mgmt_app.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_cors_beam",
        srcs = ["src/rabbit_mgmt_cors.erl"],
        outs = ["ebin/rabbit_mgmt_cors.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_csp_beam",
        srcs = ["src/rabbit_mgmt_csp.erl"],
        outs = ["ebin/rabbit_mgmt_csp.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_db_beam",
        srcs = ["src/rabbit_mgmt_db.erl"],
        outs = ["ebin/rabbit_mgmt_db.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_db_cache_beam",
        srcs = ["src/rabbit_mgmt_db_cache.erl"],
        outs = ["ebin/rabbit_mgmt_db_cache.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_db_cache_sup_beam",
        srcs = ["src/rabbit_mgmt_db_cache_sup.erl"],
        outs = ["ebin/rabbit_mgmt_db_cache_sup.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_dispatcher_beam",
        srcs = ["src/rabbit_mgmt_dispatcher.erl"],
        outs = ["ebin/rabbit_mgmt_dispatcher.beam"],
        beam = ["ebin/rabbit_mgmt_extension.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_extension_beam",
        srcs = ["src/rabbit_mgmt_extension.erl"],
        outs = ["ebin/rabbit_mgmt_extension.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_headers_beam",
        srcs = ["src/rabbit_mgmt_headers.erl"],
        outs = ["ebin/rabbit_mgmt_headers.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_hsts_beam",
        srcs = ["src/rabbit_mgmt_hsts.erl"],
        outs = ["ebin/rabbit_mgmt_hsts.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_load_definitions_beam",
        srcs = ["src/rabbit_mgmt_load_definitions.erl"],
        outs = ["ebin/rabbit_mgmt_load_definitions.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_reset_handler_beam",
        srcs = ["src/rabbit_mgmt_reset_handler.erl"],
        outs = ["ebin/rabbit_mgmt_reset_handler.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_stats_beam",
        srcs = ["src/rabbit_mgmt_stats.erl"],
        outs = ["ebin/rabbit_mgmt_stats.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_sup_beam",
        srcs = ["src/rabbit_mgmt_sup.erl"],
        outs = ["ebin/rabbit_mgmt_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_sup_sup_beam",
        srcs = ["src/rabbit_mgmt_sup_sup.erl"],
        outs = ["ebin/rabbit_mgmt_sup_sup.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_util_beam",
        srcs = ["src/rabbit_mgmt_util.erl"],
        outs = ["ebin/rabbit_mgmt_util.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_aliveness_test_beam",
        srcs = ["src/rabbit_mgmt_wm_aliveness_test.erl"],
        outs = ["ebin/rabbit_mgmt_wm_aliveness_test.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_auth_attempts_beam",
        srcs = ["src/rabbit_mgmt_wm_auth_attempts.erl"],
        outs = ["ebin/rabbit_mgmt_wm_auth_attempts.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_auth_beam",
        srcs = ["src/rabbit_mgmt_wm_auth.erl"],
        outs = ["ebin/rabbit_mgmt_wm_auth.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_binding_beam",
        srcs = ["src/rabbit_mgmt_wm_binding.erl"],
        outs = ["ebin/rabbit_mgmt_wm_binding.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_bindings_beam",
        srcs = ["src/rabbit_mgmt_wm_bindings.erl"],
        outs = ["ebin/rabbit_mgmt_wm_bindings.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_channel_beam",
        srcs = ["src/rabbit_mgmt_wm_channel.erl"],
        outs = ["ebin/rabbit_mgmt_wm_channel.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_channels_beam",
        srcs = ["src/rabbit_mgmt_wm_channels.erl"],
        outs = ["ebin/rabbit_mgmt_wm_channels.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_channels_vhost_beam",
        srcs = ["src/rabbit_mgmt_wm_channels_vhost.erl"],
        outs = ["ebin/rabbit_mgmt_wm_channels_vhost.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_cluster_name_beam",
        srcs = ["src/rabbit_mgmt_wm_cluster_name.erl"],
        outs = ["ebin/rabbit_mgmt_wm_cluster_name.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_connection_beam",
        srcs = ["src/rabbit_mgmt_wm_connection.erl"],
        outs = ["ebin/rabbit_mgmt_wm_connection.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_connection_channels_beam",
        srcs = ["src/rabbit_mgmt_wm_connection_channels.erl"],
        outs = ["ebin/rabbit_mgmt_wm_connection_channels.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_connection_user_name_beam",
        srcs = ["src/rabbit_mgmt_wm_connection_user_name.erl"],
        outs = ["ebin/rabbit_mgmt_wm_connection_user_name.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_connections_beam",
        srcs = ["src/rabbit_mgmt_wm_connections.erl"],
        outs = ["ebin/rabbit_mgmt_wm_connections.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_connections_vhost_beam",
        srcs = ["src/rabbit_mgmt_wm_connections_vhost.erl"],
        outs = ["ebin/rabbit_mgmt_wm_connections_vhost.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_consumers_beam",
        srcs = ["src/rabbit_mgmt_wm_consumers.erl"],
        outs = ["ebin/rabbit_mgmt_wm_consumers.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_definitions_beam",
        srcs = ["src/rabbit_mgmt_wm_definitions.erl"],
        outs = ["ebin/rabbit_mgmt_wm_definitions.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_exchange_beam",
        srcs = ["src/rabbit_mgmt_wm_exchange.erl"],
        outs = ["ebin/rabbit_mgmt_wm_exchange.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_exchange_publish_beam",
        srcs = ["src/rabbit_mgmt_wm_exchange_publish.erl"],
        outs = ["ebin/rabbit_mgmt_wm_exchange_publish.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_exchanges_beam",
        srcs = ["src/rabbit_mgmt_wm_exchanges.erl"],
        outs = ["ebin/rabbit_mgmt_wm_exchanges.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_extensions_beam",
        srcs = ["src/rabbit_mgmt_wm_extensions.erl"],
        outs = ["ebin/rabbit_mgmt_wm_extensions.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_feature_flag_enable_beam",
        srcs = ["src/rabbit_mgmt_wm_feature_flag_enable.erl"],
        outs = ["ebin/rabbit_mgmt_wm_feature_flag_enable.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_feature_flags_beam",
        srcs = ["src/rabbit_mgmt_wm_feature_flags.erl"],
        outs = ["ebin/rabbit_mgmt_wm_feature_flags.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_global_parameter_beam",
        srcs = ["src/rabbit_mgmt_wm_global_parameter.erl"],
        outs = ["ebin/rabbit_mgmt_wm_global_parameter.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_global_parameters_beam",
        srcs = ["src/rabbit_mgmt_wm_global_parameters.erl"],
        outs = ["ebin/rabbit_mgmt_wm_global_parameters.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_health_check_alarms_beam",
        srcs = ["src/rabbit_mgmt_wm_health_check_alarms.erl"],
        outs = ["ebin/rabbit_mgmt_wm_health_check_alarms.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_health_check_certificate_expiration_beam",
        srcs = ["src/rabbit_mgmt_wm_health_check_certificate_expiration.erl"],
        outs = ["ebin/rabbit_mgmt_wm_health_check_certificate_expiration.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_health_check_local_alarms_beam",
        srcs = ["src/rabbit_mgmt_wm_health_check_local_alarms.erl"],
        outs = ["ebin/rabbit_mgmt_wm_health_check_local_alarms.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_health_check_node_is_mirror_sync_critical_beam",
        srcs = ["src/rabbit_mgmt_wm_health_check_node_is_mirror_sync_critical.erl"],
        outs = ["ebin/rabbit_mgmt_wm_health_check_node_is_mirror_sync_critical.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_health_check_node_is_quorum_critical_beam",
        srcs = ["src/rabbit_mgmt_wm_health_check_node_is_quorum_critical.erl"],
        outs = ["ebin/rabbit_mgmt_wm_health_check_node_is_quorum_critical.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_health_check_port_listener_beam",
        srcs = ["src/rabbit_mgmt_wm_health_check_port_listener.erl"],
        outs = ["ebin/rabbit_mgmt_wm_health_check_port_listener.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_health_check_protocol_listener_beam",
        srcs = ["src/rabbit_mgmt_wm_health_check_protocol_listener.erl"],
        outs = ["ebin/rabbit_mgmt_wm_health_check_protocol_listener.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_health_check_virtual_hosts_beam",
        srcs = ["src/rabbit_mgmt_wm_health_check_virtual_hosts.erl"],
        outs = ["ebin/rabbit_mgmt_wm_health_check_virtual_hosts.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_healthchecks_beam",
        srcs = ["src/rabbit_mgmt_wm_healthchecks.erl"],
        outs = ["ebin/rabbit_mgmt_wm_healthchecks.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_limit_beam",
        srcs = ["src/rabbit_mgmt_wm_limit.erl"],
        outs = ["ebin/rabbit_mgmt_wm_limit.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_limits_beam",
        srcs = ["src/rabbit_mgmt_wm_limits.erl"],
        outs = ["ebin/rabbit_mgmt_wm_limits.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_login_beam",
        srcs = ["src/rabbit_mgmt_wm_login.erl"],
        outs = ["ebin/rabbit_mgmt_wm_login.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_node_beam",
        srcs = ["src/rabbit_mgmt_wm_node.erl"],
        outs = ["ebin/rabbit_mgmt_wm_node.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_node_memory_beam",
        srcs = ["src/rabbit_mgmt_wm_node_memory.erl"],
        outs = ["ebin/rabbit_mgmt_wm_node_memory.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_node_memory_ets_beam",
        srcs = ["src/rabbit_mgmt_wm_node_memory_ets.erl"],
        outs = ["ebin/rabbit_mgmt_wm_node_memory_ets.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_nodes_beam",
        srcs = ["src/rabbit_mgmt_wm_nodes.erl"],
        outs = ["ebin/rabbit_mgmt_wm_nodes.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_operator_policies_beam",
        srcs = ["src/rabbit_mgmt_wm_operator_policies.erl"],
        outs = ["ebin/rabbit_mgmt_wm_operator_policies.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_operator_policy_beam",
        srcs = ["src/rabbit_mgmt_wm_operator_policy.erl"],
        outs = ["ebin/rabbit_mgmt_wm_operator_policy.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_overview_beam",
        srcs = ["src/rabbit_mgmt_wm_overview.erl"],
        outs = ["ebin/rabbit_mgmt_wm_overview.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_parameter_beam",
        srcs = ["src/rabbit_mgmt_wm_parameter.erl"],
        outs = ["ebin/rabbit_mgmt_wm_parameter.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_parameters_beam",
        srcs = ["src/rabbit_mgmt_wm_parameters.erl"],
        outs = ["ebin/rabbit_mgmt_wm_parameters.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_permission_beam",
        srcs = ["src/rabbit_mgmt_wm_permission.erl"],
        outs = ["ebin/rabbit_mgmt_wm_permission.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_permissions_beam",
        srcs = ["src/rabbit_mgmt_wm_permissions.erl"],
        outs = ["ebin/rabbit_mgmt_wm_permissions.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_permissions_user_beam",
        srcs = ["src/rabbit_mgmt_wm_permissions_user.erl"],
        outs = ["ebin/rabbit_mgmt_wm_permissions_user.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_permissions_vhost_beam",
        srcs = ["src/rabbit_mgmt_wm_permissions_vhost.erl"],
        outs = ["ebin/rabbit_mgmt_wm_permissions_vhost.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_policies_beam",
        srcs = ["src/rabbit_mgmt_wm_policies.erl"],
        outs = ["ebin/rabbit_mgmt_wm_policies.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_policy_beam",
        srcs = ["src/rabbit_mgmt_wm_policy.erl"],
        outs = ["ebin/rabbit_mgmt_wm_policy.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_queue_actions_beam",
        srcs = ["src/rabbit_mgmt_wm_queue_actions.erl"],
        outs = ["ebin/rabbit_mgmt_wm_queue_actions.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_queue_beam",
        srcs = ["src/rabbit_mgmt_wm_queue.erl"],
        outs = ["ebin/rabbit_mgmt_wm_queue.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_queue_get_beam",
        srcs = ["src/rabbit_mgmt_wm_queue_get.erl"],
        outs = ["ebin/rabbit_mgmt_wm_queue_get.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_queue_purge_beam",
        srcs = ["src/rabbit_mgmt_wm_queue_purge.erl"],
        outs = ["ebin/rabbit_mgmt_wm_queue_purge.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_queues_beam",
        srcs = ["src/rabbit_mgmt_wm_queues.erl"],
        outs = ["ebin/rabbit_mgmt_wm_queues.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_rebalance_queues_beam",
        srcs = ["src/rabbit_mgmt_wm_rebalance_queues.erl"],
        outs = ["ebin/rabbit_mgmt_wm_rebalance_queues.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_redirect_beam",
        srcs = ["src/rabbit_mgmt_wm_redirect.erl"],
        outs = ["ebin/rabbit_mgmt_wm_redirect.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_reset_beam",
        srcs = ["src/rabbit_mgmt_wm_reset.erl"],
        outs = ["ebin/rabbit_mgmt_wm_reset.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_static_beam",
        srcs = ["src/rabbit_mgmt_wm_static.erl"],
        outs = ["ebin/rabbit_mgmt_wm_static.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_topic_permission_beam",
        srcs = ["src/rabbit_mgmt_wm_topic_permission.erl"],
        outs = ["ebin/rabbit_mgmt_wm_topic_permission.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_topic_permissions_beam",
        srcs = ["src/rabbit_mgmt_wm_topic_permissions.erl"],
        outs = ["ebin/rabbit_mgmt_wm_topic_permissions.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_topic_permissions_user_beam",
        srcs = ["src/rabbit_mgmt_wm_topic_permissions_user.erl"],
        outs = ["ebin/rabbit_mgmt_wm_topic_permissions_user.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_topic_permissions_vhost_beam",
        srcs = ["src/rabbit_mgmt_wm_topic_permissions_vhost.erl"],
        outs = ["ebin/rabbit_mgmt_wm_topic_permissions_vhost.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_user_beam",
        srcs = ["src/rabbit_mgmt_wm_user.erl"],
        outs = ["ebin/rabbit_mgmt_wm_user.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_user_limit_beam",
        srcs = ["src/rabbit_mgmt_wm_user_limit.erl"],
        outs = ["ebin/rabbit_mgmt_wm_user_limit.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_user_limits_beam",
        srcs = ["src/rabbit_mgmt_wm_user_limits.erl"],
        outs = ["ebin/rabbit_mgmt_wm_user_limits.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_users_beam",
        srcs = ["src/rabbit_mgmt_wm_users.erl"],
        outs = ["ebin/rabbit_mgmt_wm_users.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_users_bulk_delete_beam",
        srcs = ["src/rabbit_mgmt_wm_users_bulk_delete.erl"],
        outs = ["ebin/rabbit_mgmt_wm_users_bulk_delete.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_vhost_beam",
        srcs = ["src/rabbit_mgmt_wm_vhost.erl"],
        outs = ["ebin/rabbit_mgmt_wm_vhost.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_vhost_restart_beam",
        srcs = ["src/rabbit_mgmt_wm_vhost_restart.erl"],
        outs = ["ebin/rabbit_mgmt_wm_vhost_restart.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_vhosts_beam",
        srcs = ["src/rabbit_mgmt_wm_vhosts.erl"],
        outs = ["ebin/rabbit_mgmt_wm_vhosts.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mgmt_wm_whoami_beam",
        srcs = ["src/rabbit_mgmt_wm_whoami.erl"],
        outs = ["ebin/rabbit_mgmt_wm_whoami.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_mgmt_app.beam", "test/rabbit_mgmt_cors.beam", "test/rabbit_mgmt_csp.beam", "test/rabbit_mgmt_db.beam", "test/rabbit_mgmt_db_cache.beam", "test/rabbit_mgmt_db_cache_sup.beam", "test/rabbit_mgmt_dispatcher.beam", "test/rabbit_mgmt_extension.beam", "test/rabbit_mgmt_headers.beam", "test/rabbit_mgmt_hsts.beam", "test/rabbit_mgmt_load_definitions.beam", "test/rabbit_mgmt_reset_handler.beam", "test/rabbit_mgmt_stats.beam", "test/rabbit_mgmt_sup.beam", "test/rabbit_mgmt_sup_sup.beam", "test/rabbit_mgmt_util.beam", "test/rabbit_mgmt_wm_aliveness_test.beam", "test/rabbit_mgmt_wm_auth.beam", "test/rabbit_mgmt_wm_auth_attempts.beam", "test/rabbit_mgmt_wm_binding.beam", "test/rabbit_mgmt_wm_bindings.beam", "test/rabbit_mgmt_wm_channel.beam", "test/rabbit_mgmt_wm_channels.beam", "test/rabbit_mgmt_wm_channels_vhost.beam", "test/rabbit_mgmt_wm_cluster_name.beam", "test/rabbit_mgmt_wm_connection.beam", "test/rabbit_mgmt_wm_connection_channels.beam", "test/rabbit_mgmt_wm_connection_user_name.beam", "test/rabbit_mgmt_wm_connections.beam", "test/rabbit_mgmt_wm_connections_vhost.beam", "test/rabbit_mgmt_wm_consumers.beam", "test/rabbit_mgmt_wm_definitions.beam", "test/rabbit_mgmt_wm_exchange.beam", "test/rabbit_mgmt_wm_exchange_publish.beam", "test/rabbit_mgmt_wm_exchanges.beam", "test/rabbit_mgmt_wm_extensions.beam", "test/rabbit_mgmt_wm_feature_flag_enable.beam", "test/rabbit_mgmt_wm_feature_flags.beam", "test/rabbit_mgmt_wm_global_parameter.beam", "test/rabbit_mgmt_wm_global_parameters.beam", "test/rabbit_mgmt_wm_health_check_alarms.beam", "test/rabbit_mgmt_wm_health_check_certificate_expiration.beam", "test/rabbit_mgmt_wm_health_check_local_alarms.beam", "test/rabbit_mgmt_wm_health_check_node_is_mirror_sync_critical.beam", "test/rabbit_mgmt_wm_health_check_node_is_quorum_critical.beam", "test/rabbit_mgmt_wm_health_check_port_listener.beam", "test/rabbit_mgmt_wm_health_check_protocol_listener.beam", "test/rabbit_mgmt_wm_health_check_virtual_hosts.beam", "test/rabbit_mgmt_wm_healthchecks.beam", "test/rabbit_mgmt_wm_limit.beam", "test/rabbit_mgmt_wm_limits.beam", "test/rabbit_mgmt_wm_login.beam", "test/rabbit_mgmt_wm_node.beam", "test/rabbit_mgmt_wm_node_memory.beam", "test/rabbit_mgmt_wm_node_memory_ets.beam", "test/rabbit_mgmt_wm_nodes.beam", "test/rabbit_mgmt_wm_operator_policies.beam", "test/rabbit_mgmt_wm_operator_policy.beam", "test/rabbit_mgmt_wm_overview.beam", "test/rabbit_mgmt_wm_parameter.beam", "test/rabbit_mgmt_wm_parameters.beam", "test/rabbit_mgmt_wm_permission.beam", "test/rabbit_mgmt_wm_permissions.beam", "test/rabbit_mgmt_wm_permissions_user.beam", "test/rabbit_mgmt_wm_permissions_vhost.beam", "test/rabbit_mgmt_wm_policies.beam", "test/rabbit_mgmt_wm_policy.beam", "test/rabbit_mgmt_wm_queue.beam", "test/rabbit_mgmt_wm_queue_actions.beam", "test/rabbit_mgmt_wm_queue_get.beam", "test/rabbit_mgmt_wm_queue_purge.beam", "test/rabbit_mgmt_wm_queues.beam", "test/rabbit_mgmt_wm_rebalance_queues.beam", "test/rabbit_mgmt_wm_redirect.beam", "test/rabbit_mgmt_wm_reset.beam", "test/rabbit_mgmt_wm_static.beam", "test/rabbit_mgmt_wm_topic_permission.beam", "test/rabbit_mgmt_wm_topic_permissions.beam", "test/rabbit_mgmt_wm_topic_permissions_user.beam", "test/rabbit_mgmt_wm_topic_permissions_vhost.beam", "test/rabbit_mgmt_wm_user.beam", "test/rabbit_mgmt_wm_user_limit.beam", "test/rabbit_mgmt_wm_user_limits.beam", "test/rabbit_mgmt_wm_users.beam", "test/rabbit_mgmt_wm_users_bulk_delete.beam", "test/rabbit_mgmt_wm_vhost.beam", "test/rabbit_mgmt_wm_vhost_restart.beam", "test/rabbit_mgmt_wm_vhosts.beam", "test/rabbit_mgmt_wm_whoami.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_app_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_app.erl"],
        outs = ["test/rabbit_mgmt_app.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_cors_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_cors.erl"],
        outs = ["test/rabbit_mgmt_cors.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_csp_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_csp.erl"],
        outs = ["test/rabbit_mgmt_csp.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_db_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_db.erl"],
        outs = ["test/rabbit_mgmt_db.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_db_cache_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_db_cache.erl"],
        outs = ["test/rabbit_mgmt_db_cache.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_db_cache_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_db_cache_sup.erl"],
        outs = ["test/rabbit_mgmt_db_cache_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_dispatcher_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_dispatcher.erl"],
        outs = ["test/rabbit_mgmt_dispatcher.beam"],
        beam = ["ebin/rabbit_mgmt_extension.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_extension_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_extension.erl"],
        outs = ["test/rabbit_mgmt_extension.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_headers_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_headers.erl"],
        outs = ["test/rabbit_mgmt_headers.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_hsts_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_hsts.erl"],
        outs = ["test/rabbit_mgmt_hsts.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_load_definitions_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_load_definitions.erl"],
        outs = ["test/rabbit_mgmt_load_definitions.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_reset_handler_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_reset_handler.erl"],
        outs = ["test/rabbit_mgmt_reset_handler.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_stats_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_stats.erl"],
        outs = ["test/rabbit_mgmt_stats.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_sup.erl"],
        outs = ["test/rabbit_mgmt_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_sup_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_sup_sup.erl"],
        outs = ["test/rabbit_mgmt_sup_sup.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_util_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_util.erl"],
        outs = ["test/rabbit_mgmt_util.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_aliveness_test_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_aliveness_test.erl"],
        outs = ["test/rabbit_mgmt_wm_aliveness_test.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_auth_attempts_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_auth_attempts.erl"],
        outs = ["test/rabbit_mgmt_wm_auth_attempts.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_auth_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_auth.erl"],
        outs = ["test/rabbit_mgmt_wm_auth.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_binding_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_binding.erl"],
        outs = ["test/rabbit_mgmt_wm_binding.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_bindings_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_bindings.erl"],
        outs = ["test/rabbit_mgmt_wm_bindings.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_channel_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_channel.erl"],
        outs = ["test/rabbit_mgmt_wm_channel.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_channels_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_channels.erl"],
        outs = ["test/rabbit_mgmt_wm_channels.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_channels_vhost_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_channels_vhost.erl"],
        outs = ["test/rabbit_mgmt_wm_channels_vhost.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_cluster_name_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_cluster_name.erl"],
        outs = ["test/rabbit_mgmt_wm_cluster_name.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_connection_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_connection.erl"],
        outs = ["test/rabbit_mgmt_wm_connection.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_connection_channels_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_connection_channels.erl"],
        outs = ["test/rabbit_mgmt_wm_connection_channels.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_connection_user_name_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_connection_user_name.erl"],
        outs = ["test/rabbit_mgmt_wm_connection_user_name.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_connections_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_connections.erl"],
        outs = ["test/rabbit_mgmt_wm_connections.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_connections_vhost_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_connections_vhost.erl"],
        outs = ["test/rabbit_mgmt_wm_connections_vhost.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_consumers_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_consumers.erl"],
        outs = ["test/rabbit_mgmt_wm_consumers.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_definitions_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_definitions.erl"],
        outs = ["test/rabbit_mgmt_wm_definitions.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_exchange_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_exchange.erl"],
        outs = ["test/rabbit_mgmt_wm_exchange.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_exchange_publish_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_exchange_publish.erl"],
        outs = ["test/rabbit_mgmt_wm_exchange_publish.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_exchanges_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_exchanges.erl"],
        outs = ["test/rabbit_mgmt_wm_exchanges.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_extensions_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_extensions.erl"],
        outs = ["test/rabbit_mgmt_wm_extensions.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_feature_flag_enable_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_feature_flag_enable.erl"],
        outs = ["test/rabbit_mgmt_wm_feature_flag_enable.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_feature_flags_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_feature_flags.erl"],
        outs = ["test/rabbit_mgmt_wm_feature_flags.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_global_parameter_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_global_parameter.erl"],
        outs = ["test/rabbit_mgmt_wm_global_parameter.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_global_parameters_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_global_parameters.erl"],
        outs = ["test/rabbit_mgmt_wm_global_parameters.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_health_check_alarms_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_health_check_alarms.erl"],
        outs = ["test/rabbit_mgmt_wm_health_check_alarms.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_health_check_certificate_expiration_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_health_check_certificate_expiration.erl"],
        outs = ["test/rabbit_mgmt_wm_health_check_certificate_expiration.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_health_check_local_alarms_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_health_check_local_alarms.erl"],
        outs = ["test/rabbit_mgmt_wm_health_check_local_alarms.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_health_check_node_is_mirror_sync_critical_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_health_check_node_is_mirror_sync_critical.erl"],
        outs = ["test/rabbit_mgmt_wm_health_check_node_is_mirror_sync_critical.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_health_check_node_is_quorum_critical_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_health_check_node_is_quorum_critical.erl"],
        outs = ["test/rabbit_mgmt_wm_health_check_node_is_quorum_critical.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_health_check_port_listener_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_health_check_port_listener.erl"],
        outs = ["test/rabbit_mgmt_wm_health_check_port_listener.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_health_check_protocol_listener_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_health_check_protocol_listener.erl"],
        outs = ["test/rabbit_mgmt_wm_health_check_protocol_listener.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_health_check_virtual_hosts_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_health_check_virtual_hosts.erl"],
        outs = ["test/rabbit_mgmt_wm_health_check_virtual_hosts.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_healthchecks_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_healthchecks.erl"],
        outs = ["test/rabbit_mgmt_wm_healthchecks.beam"],
        hdrs = ["include/rabbit_mgmt.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_limit_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_limit.erl"],
        outs = ["test/rabbit_mgmt_wm_limit.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_limits_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_limits.erl"],
        outs = ["test/rabbit_mgmt_wm_limits.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_login_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_login.erl"],
        outs = ["test/rabbit_mgmt_wm_login.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_node_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_node.erl"],
        outs = ["test/rabbit_mgmt_wm_node.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_node_memory_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_node_memory.erl"],
        outs = ["test/rabbit_mgmt_wm_node_memory.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_node_memory_ets_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_node_memory_ets.erl"],
        outs = ["test/rabbit_mgmt_wm_node_memory_ets.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_nodes_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_nodes.erl"],
        outs = ["test/rabbit_mgmt_wm_nodes.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_operator_policies_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_operator_policies.erl"],
        outs = ["test/rabbit_mgmt_wm_operator_policies.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_operator_policy_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_operator_policy.erl"],
        outs = ["test/rabbit_mgmt_wm_operator_policy.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_overview_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_overview.erl"],
        outs = ["test/rabbit_mgmt_wm_overview.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_parameter_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_parameter.erl"],
        outs = ["test/rabbit_mgmt_wm_parameter.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_parameters_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_parameters.erl"],
        outs = ["test/rabbit_mgmt_wm_parameters.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_permission_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_permission.erl"],
        outs = ["test/rabbit_mgmt_wm_permission.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_permissions_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_permissions.erl"],
        outs = ["test/rabbit_mgmt_wm_permissions.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_permissions_user_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_permissions_user.erl"],
        outs = ["test/rabbit_mgmt_wm_permissions_user.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_permissions_vhost_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_permissions_vhost.erl"],
        outs = ["test/rabbit_mgmt_wm_permissions_vhost.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_policies_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_policies.erl"],
        outs = ["test/rabbit_mgmt_wm_policies.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_policy_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_policy.erl"],
        outs = ["test/rabbit_mgmt_wm_policy.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_queue_actions_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_queue_actions.erl"],
        outs = ["test/rabbit_mgmt_wm_queue_actions.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbit:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_queue_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_queue.erl"],
        outs = ["test/rabbit_mgmt_wm_queue.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_queue_get_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_queue_get.erl"],
        outs = ["test/rabbit_mgmt_wm_queue_get.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_queue_purge_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_queue_purge.erl"],
        outs = ["test/rabbit_mgmt_wm_queue_purge.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_queues_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_queues.erl"],
        outs = ["test/rabbit_mgmt_wm_queues.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_rebalance_queues_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_rebalance_queues.erl"],
        outs = ["test/rabbit_mgmt_wm_rebalance_queues.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_redirect_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_redirect.erl"],
        outs = ["test/rabbit_mgmt_wm_redirect.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_reset_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_reset.erl"],
        outs = ["test/rabbit_mgmt_wm_reset.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_static_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_static.erl"],
        outs = ["test/rabbit_mgmt_wm_static.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_topic_permission_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_topic_permission.erl"],
        outs = ["test/rabbit_mgmt_wm_topic_permission.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_topic_permissions_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_topic_permissions.erl"],
        outs = ["test/rabbit_mgmt_wm_topic_permissions.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_topic_permissions_user_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_topic_permissions_user.erl"],
        outs = ["test/rabbit_mgmt_wm_topic_permissions_user.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_topic_permissions_vhost_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_topic_permissions_vhost.erl"],
        outs = ["test/rabbit_mgmt_wm_topic_permissions_vhost.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_user_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_user.erl"],
        outs = ["test/rabbit_mgmt_wm_user.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_user_limit_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_user_limit.erl"],
        outs = ["test/rabbit_mgmt_wm_user_limit.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_user_limits_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_user_limits.erl"],
        outs = ["test/rabbit_mgmt_wm_user_limits.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_users_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_users.erl"],
        outs = ["test/rabbit_mgmt_wm_users.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_users_bulk_delete_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_users_bulk_delete.erl"],
        outs = ["test/rabbit_mgmt_wm_users_bulk_delete.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_vhost_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_vhost.erl"],
        outs = ["test/rabbit_mgmt_wm_vhost.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_vhost_restart_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_vhost_restart.erl"],
        outs = ["test/rabbit_mgmt_wm_vhost_restart.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_vhosts_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_vhosts.erl"],
        outs = ["test/rabbit_mgmt_wm_vhosts.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mgmt_wm_whoami_beam",
        testonly = True,
        srcs = ["src/rabbit_mgmt_wm_whoami.erl"],
        outs = ["test/rabbit_mgmt_wm_whoami.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "//deps/rabbitmq_management_agent:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/rabbit_mgmt.hrl", "src/rabbit_mgmt_app.erl", "src/rabbit_mgmt_cors.erl", "src/rabbit_mgmt_csp.erl", "src/rabbit_mgmt_db.erl", "src/rabbit_mgmt_db_cache.erl", "src/rabbit_mgmt_db_cache_sup.erl", "src/rabbit_mgmt_dispatcher.erl", "src/rabbit_mgmt_extension.erl", "src/rabbit_mgmt_headers.erl", "src/rabbit_mgmt_hsts.erl", "src/rabbit_mgmt_load_definitions.erl", "src/rabbit_mgmt_reset_handler.erl", "src/rabbit_mgmt_stats.erl", "src/rabbit_mgmt_sup.erl", "src/rabbit_mgmt_sup_sup.erl", "src/rabbit_mgmt_util.erl", "src/rabbit_mgmt_wm_aliveness_test.erl", "src/rabbit_mgmt_wm_auth.erl", "src/rabbit_mgmt_wm_auth_attempts.erl", "src/rabbit_mgmt_wm_binding.erl", "src/rabbit_mgmt_wm_bindings.erl", "src/rabbit_mgmt_wm_channel.erl", "src/rabbit_mgmt_wm_channels.erl", "src/rabbit_mgmt_wm_channels_vhost.erl", "src/rabbit_mgmt_wm_cluster_name.erl", "src/rabbit_mgmt_wm_connection.erl", "src/rabbit_mgmt_wm_connection_channels.erl", "src/rabbit_mgmt_wm_connection_user_name.erl", "src/rabbit_mgmt_wm_connections.erl", "src/rabbit_mgmt_wm_connections_vhost.erl", "src/rabbit_mgmt_wm_consumers.erl", "src/rabbit_mgmt_wm_definitions.erl", "src/rabbit_mgmt_wm_exchange.erl", "src/rabbit_mgmt_wm_exchange_publish.erl", "src/rabbit_mgmt_wm_exchanges.erl", "src/rabbit_mgmt_wm_extensions.erl", "src/rabbit_mgmt_wm_feature_flag_enable.erl", "src/rabbit_mgmt_wm_feature_flags.erl", "src/rabbit_mgmt_wm_global_parameter.erl", "src/rabbit_mgmt_wm_global_parameters.erl", "src/rabbit_mgmt_wm_health_check_alarms.erl", "src/rabbit_mgmt_wm_health_check_certificate_expiration.erl", "src/rabbit_mgmt_wm_health_check_local_alarms.erl", "src/rabbit_mgmt_wm_health_check_node_is_mirror_sync_critical.erl", "src/rabbit_mgmt_wm_health_check_node_is_quorum_critical.erl", "src/rabbit_mgmt_wm_health_check_port_listener.erl", "src/rabbit_mgmt_wm_health_check_protocol_listener.erl", "src/rabbit_mgmt_wm_health_check_virtual_hosts.erl", "src/rabbit_mgmt_wm_healthchecks.erl", "src/rabbit_mgmt_wm_limit.erl", "src/rabbit_mgmt_wm_limits.erl", "src/rabbit_mgmt_wm_login.erl", "src/rabbit_mgmt_wm_node.erl", "src/rabbit_mgmt_wm_node_memory.erl", "src/rabbit_mgmt_wm_node_memory_ets.erl", "src/rabbit_mgmt_wm_nodes.erl", "src/rabbit_mgmt_wm_operator_policies.erl", "src/rabbit_mgmt_wm_operator_policy.erl", "src/rabbit_mgmt_wm_overview.erl", "src/rabbit_mgmt_wm_parameter.erl", "src/rabbit_mgmt_wm_parameters.erl", "src/rabbit_mgmt_wm_permission.erl", "src/rabbit_mgmt_wm_permissions.erl", "src/rabbit_mgmt_wm_permissions_user.erl", "src/rabbit_mgmt_wm_permissions_vhost.erl", "src/rabbit_mgmt_wm_policies.erl", "src/rabbit_mgmt_wm_policy.erl", "src/rabbit_mgmt_wm_queue.erl", "src/rabbit_mgmt_wm_queue_actions.erl", "src/rabbit_mgmt_wm_queue_get.erl", "src/rabbit_mgmt_wm_queue_purge.erl", "src/rabbit_mgmt_wm_queues.erl", "src/rabbit_mgmt_wm_rebalance_queues.erl", "src/rabbit_mgmt_wm_redirect.erl", "src/rabbit_mgmt_wm_reset.erl", "src/rabbit_mgmt_wm_static.erl", "src/rabbit_mgmt_wm_topic_permission.erl", "src/rabbit_mgmt_wm_topic_permissions.erl", "src/rabbit_mgmt_wm_topic_permissions_user.erl", "src/rabbit_mgmt_wm_topic_permissions_vhost.erl", "src/rabbit_mgmt_wm_user.erl", "src/rabbit_mgmt_wm_user_limit.erl", "src/rabbit_mgmt_wm_user_limits.erl", "src/rabbit_mgmt_wm_users.erl", "src/rabbit_mgmt_wm_users_bulk_delete.erl", "src/rabbit_mgmt_wm_vhost.erl", "src/rabbit_mgmt_wm_vhost_restart.erl", "src/rabbit_mgmt_wm_vhosts.erl", "src/rabbit_mgmt_wm_whoami.erl"],
    )
