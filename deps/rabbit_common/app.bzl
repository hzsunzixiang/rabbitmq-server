load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    erlang_bytecode(
        name = "ebin_app_utils_beam",
        srcs = ["src/app_utils.erl"],
        outs = ["ebin/app_utils.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_code_version_beam",
        srcs = ["src/code_version.erl"],
        outs = ["ebin/code_version.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_credit_flow_beam",
        srcs = ["src/credit_flow.erl"],
        outs = ["ebin/credit_flow.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_delegate_beam",
        srcs = ["src/delegate.erl"],
        outs = ["ebin/delegate.beam"],
        beam = ["ebin/gen_server2.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_delegate_sup_beam",
        srcs = ["src/delegate_sup.erl"],
        outs = ["ebin/delegate_sup.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_file_handle_cache_beam",
        srcs = ["src/file_handle_cache.erl"],
        outs = ["ebin/file_handle_cache.beam"],
        beam = ["ebin/gen_server2.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_file_handle_cache_stats_beam",
        srcs = ["src/file_handle_cache_stats.erl"],
        outs = ["ebin/file_handle_cache_stats.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_gen_server2_beam",
        srcs = ["src/gen_server2.erl"],
        outs = ["ebin/gen_server2.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_mirrored_supervisor_locks_beam",
        srcs = ["src/mirrored_supervisor_locks.erl"],
        outs = ["ebin/mirrored_supervisor_locks.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_mnesia_sync_beam",
        srcs = ["src/mnesia_sync.erl"],
        outs = ["ebin/mnesia_sync.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_pmon_beam",
        srcs = ["src/pmon.erl"],
        outs = ["ebin/pmon.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_priority_queue_beam",
        srcs = ["src/priority_queue.erl"],
        outs = ["ebin/priority_queue.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqp_connection_beam",
        srcs = ["src/rabbit_amqp_connection.erl"],
        outs = ["ebin/rabbit_amqp_connection.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqqueue_common_beam",
        srcs = ["src/rabbit_amqqueue_common.erl"],
        outs = ["ebin/rabbit_amqqueue_common.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_backend_dummy_beam",
        srcs = ["src/rabbit_auth_backend_dummy.erl"],
        outs = ["ebin/rabbit_auth_backend_dummy.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        beam = ["ebin/rabbit_authn_backend.beam", "ebin/rabbit_authz_backend.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_mechanism_beam",
        srcs = ["src/rabbit_auth_mechanism.erl"],
        outs = ["ebin/rabbit_auth_mechanism.beam"],
        beam = ["ebin/rabbit_registry_class.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_authn_backend_beam",
        srcs = ["src/rabbit_authn_backend.erl"],
        outs = ["ebin/rabbit_authn_backend.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_authz_backend_beam",
        srcs = ["src/rabbit_authz_backend.erl"],
        outs = ["ebin/rabbit_authz_backend.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_basic_common_beam",
        srcs = ["src/rabbit_basic_common.erl"],
        outs = ["ebin/rabbit_basic_common.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_binary_generator_beam",
        srcs = ["src/rabbit_binary_generator.erl"],
        outs = ["ebin/rabbit_binary_generator.beam"],
        hdrs = ["include/rabbit.hrl", "include/rabbit_framing.hrl", "include/resource.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_binary_parser_beam",
        srcs = ["src/rabbit_binary_parser.erl"],
        outs = ["ebin/rabbit_binary_parser.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_cert_info_beam",
        srcs = ["src/rabbit_cert_info.erl"],
        outs = ["ebin/rabbit_cert_info.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_channel_common_beam",
        srcs = ["src/rabbit_channel_common.erl"],
        outs = ["ebin/rabbit_channel_common.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_command_assembler_beam",
        srcs = ["src/rabbit_command_assembler.erl"],
        outs = ["ebin/rabbit_command_assembler.beam"],
        hdrs = ["include/rabbit.hrl", "include/rabbit_framing.hrl", "include/resource.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_control_misc_beam",
        srcs = ["src/rabbit_control_misc.erl"],
        outs = ["ebin/rabbit_control_misc.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_core_metrics_beam",
        srcs = ["src/rabbit_core_metrics.erl"],
        outs = ["ebin/rabbit_core_metrics.beam"],
        hdrs = ["include/rabbit_core_metrics.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_data_coercion_beam",
        srcs = ["src/rabbit_data_coercion.erl"],
        outs = ["ebin/rabbit_data_coercion.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_date_time_beam",
        srcs = ["src/rabbit_date_time.erl"],
        outs = ["ebin/rabbit_date_time.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_env_beam",
        srcs = ["src/rabbit_env.erl"],
        outs = ["ebin/rabbit_env.beam"],
        hdrs = ["include/logging.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_error_logger_handler_beam",
        srcs = ["src/rabbit_error_logger_handler.erl"],
        outs = ["ebin/rabbit_error_logger_handler.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_event_beam",
        srcs = ["src/rabbit_event.erl"],
        outs = ["ebin/rabbit_event.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_exchange_type_beam",
        srcs = ["src/rabbit_exchange_type.erl"],
        outs = ["ebin/rabbit_exchange_type.beam"],
        beam = ["ebin/rabbit_registry_class.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_heartbeat_beam",
        srcs = ["src/rabbit_heartbeat.erl"],
        outs = ["ebin/rabbit_heartbeat.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_http_util_beam",
        srcs = ["src/rabbit_http_util.erl"],
        outs = ["ebin/rabbit_http_util.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_json_beam",
        srcs = ["src/rabbit_json.erl"],
        outs = ["ebin/rabbit_json.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_log_beam",
        srcs = ["src/rabbit_log.erl"],
        outs = ["ebin/rabbit_log.beam"],
        hdrs = ["include/logging.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_misc_beam",
        srcs = ["src/rabbit_misc.erl"],
        outs = ["ebin/rabbit_misc.beam"],
        hdrs = ["include/rabbit.hrl", "include/rabbit_framing.hrl", "include/rabbit_misc.hrl", "include/resource.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_msg_store_index_beam",
        srcs = ["src/rabbit_msg_store_index.erl"],
        outs = ["ebin/rabbit_msg_store_index.beam"],
        hdrs = ["include/rabbit.hrl", "include/rabbit_msg_store.hrl", "include/resource.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_net_beam",
        srcs = ["src/rabbit_net.erl"],
        outs = ["ebin/rabbit_net.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_nodes_common_beam",
        srcs = ["src/rabbit_nodes_common.erl"],
        outs = ["ebin/rabbit_nodes_common.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_numerical_beam",
        srcs = ["src/rabbit_numerical.erl"],
        outs = ["ebin/rabbit_numerical.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_password_hashing_beam",
        srcs = ["src/rabbit_password_hashing.erl"],
        outs = ["ebin/rabbit_password_hashing.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_pbe_beam",
        srcs = ["src/rabbit_pbe.erl"],
        outs = ["ebin/rabbit_pbe.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_peer_discovery_backend_beam",
        srcs = ["src/rabbit_peer_discovery_backend.erl"],
        outs = ["ebin/rabbit_peer_discovery_backend.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_policy_validator_beam",
        srcs = ["src/rabbit_policy_validator.erl"],
        outs = ["ebin/rabbit_policy_validator.beam"],
        beam = ["ebin/rabbit_registry_class.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_queue_collector_beam",
        srcs = ["src/rabbit_queue_collector.erl"],
        outs = ["ebin/rabbit_queue_collector.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_registry_beam",
        srcs = ["src/rabbit_registry.erl"],
        outs = ["ebin/rabbit_registry.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_registry_class_beam",
        srcs = ["src/rabbit_registry_class.erl"],
        outs = ["ebin/rabbit_registry_class.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_resource_monitor_misc_beam",
        srcs = ["src/rabbit_resource_monitor_misc.erl"],
        outs = ["ebin/rabbit_resource_monitor_misc.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_runtime_beam",
        srcs = ["src/rabbit_runtime.erl"],
        outs = ["ebin/rabbit_runtime.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_runtime_parameter_beam",
        srcs = ["src/rabbit_runtime_parameter.erl"],
        outs = ["ebin/rabbit_runtime_parameter.beam"],
        beam = ["ebin/rabbit_registry_class.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_semver_beam",
        srcs = ["src/rabbit_semver.erl"],
        outs = ["ebin/rabbit_semver.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_semver_parser_beam",
        srcs = ["src/rabbit_semver_parser.erl"],
        outs = ["ebin/rabbit_semver_parser.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_ssl_options_beam",
        srcs = ["src/rabbit_ssl_options.erl"],
        outs = ["ebin/rabbit_ssl_options.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_types_beam",
        srcs = ["src/rabbit_types.erl"],
        outs = ["ebin/rabbit_types.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_writer_beam",
        srcs = ["src/rabbit_writer.erl"],
        outs = ["ebin/rabbit_writer.beam"],
        hdrs = ["include/rabbit.hrl", "include/rabbit_framing.hrl", "include/resource.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_supervisor2_beam",
        srcs = ["src/supervisor2.erl"],
        outs = ["ebin/supervisor2.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_vm_memory_monitor_beam",
        srcs = ["src/vm_memory_monitor.erl"],
        outs = ["ebin/vm_memory_monitor.beam"],
        hdrs = ["include/rabbit_memory.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_worker_pool_beam",
        srcs = ["src/worker_pool.erl"],
        outs = ["ebin/worker_pool.beam"],
        beam = ["ebin/gen_server2.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_worker_pool_sup_beam",
        srcs = ["src/worker_pool_sup.erl"],
        outs = ["ebin/worker_pool_sup.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_worker_pool_worker_beam",
        srcs = ["src/worker_pool_worker.erl"],
        outs = ["ebin/worker_pool_worker.beam"],
        beam = ["ebin/gen_server2.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_framing_amqp_0_9_1_beam",
        srcs = ["src/rabbit_framing_amqp_0_9_1.erl"],
        outs = ["ebin/rabbit_framing_amqp_0_9_1.beam"],
        hdrs = ["include/rabbit_framing.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_framing_amqp_0_8_beam",
        srcs = ["src/rabbit_framing_amqp_0_8.erl"],
        outs = ["ebin/rabbit_framing_amqp_0_8.beam"],
        hdrs = ["include/rabbit_framing.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    filegroup(
        name = "beam_files",
        srcs = ["ebin/app_utils.beam", "ebin/code_version.beam", "ebin/credit_flow.beam", "ebin/delegate.beam", "ebin/delegate_sup.beam", "ebin/file_handle_cache.beam", "ebin/file_handle_cache_stats.beam", "ebin/gen_server2.beam", "ebin/mirrored_supervisor_locks.beam", "ebin/mnesia_sync.beam", "ebin/pmon.beam", "ebin/priority_queue.beam", "ebin/rabbit_amqp_connection.beam", "ebin/rabbit_amqqueue_common.beam", "ebin/rabbit_auth_backend_dummy.beam", "ebin/rabbit_auth_mechanism.beam", "ebin/rabbit_authn_backend.beam", "ebin/rabbit_authz_backend.beam", "ebin/rabbit_basic_common.beam", "ebin/rabbit_binary_generator.beam", "ebin/rabbit_binary_parser.beam", "ebin/rabbit_cert_info.beam", "ebin/rabbit_channel_common.beam", "ebin/rabbit_command_assembler.beam", "ebin/rabbit_control_misc.beam", "ebin/rabbit_core_metrics.beam", "ebin/rabbit_data_coercion.beam", "ebin/rabbit_date_time.beam", "ebin/rabbit_env.beam", "ebin/rabbit_error_logger_handler.beam", "ebin/rabbit_event.beam", "ebin/rabbit_exchange_type.beam", "ebin/rabbit_framing_amqp_0_8.beam", "ebin/rabbit_framing_amqp_0_9_1.beam", "ebin/rabbit_heartbeat.beam", "ebin/rabbit_http_util.beam", "ebin/rabbit_json.beam", "ebin/rabbit_log.beam", "ebin/rabbit_misc.beam", "ebin/rabbit_msg_store_index.beam", "ebin/rabbit_net.beam", "ebin/rabbit_nodes_common.beam", "ebin/rabbit_numerical.beam", "ebin/rabbit_password_hashing.beam", "ebin/rabbit_pbe.beam", "ebin/rabbit_peer_discovery_backend.beam", "ebin/rabbit_policy_validator.beam", "ebin/rabbit_queue_collector.beam", "ebin/rabbit_registry.beam", "ebin/rabbit_registry_class.beam", "ebin/rabbit_resource_monitor_misc.beam", "ebin/rabbit_runtime.beam", "ebin/rabbit_runtime_parameter.beam", "ebin/rabbit_semver.beam", "ebin/rabbit_semver_parser.beam", "ebin/rabbit_ssl_options.beam", "ebin/rabbit_types.beam", "ebin/rabbit_writer.beam", "ebin/supervisor2.beam", "ebin/vm_memory_monitor.beam", "ebin/worker_pool.beam", "ebin/worker_pool_sup.beam", "ebin/worker_pool_worker.beam"],
    )

def all_test_beam_files():
    erlang_bytecode(
        name = "test_app_utils_beam",
        testonly = True,
        srcs = ["src/app_utils.erl"],
        outs = ["test/app_utils.beam"],
        erlc_opts = "//:test_erlc_opts",
    )

    erlang_bytecode(
        name = "test_code_version_beam",
        testonly = True,
        srcs = ["src/code_version.erl"],
        outs = ["test/code_version.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_credit_flow_beam",
        testonly = True,
        srcs = ["src/credit_flow.erl"],
        outs = ["test/credit_flow.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_delegate_beam",
        testonly = True,
        srcs = ["src/delegate.erl"],
        outs = ["test/delegate.beam"],
        beam = ["ebin/gen_server2.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_delegate_sup_beam",
        testonly = True,
        srcs = ["src/delegate_sup.erl"],
        outs = ["test/delegate_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_file_handle_cache_beam",
        testonly = True,
        srcs = ["src/file_handle_cache.erl"],
        outs = ["test/file_handle_cache.beam"],
        beam = ["ebin/gen_server2.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_file_handle_cache_stats_beam",
        testonly = True,
        srcs = ["src/file_handle_cache_stats.erl"],
        outs = ["test/file_handle_cache_stats.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_gen_server2_beam",
        testonly = True,
        srcs = ["src/gen_server2.erl"],
        outs = ["test/gen_server2.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_mirrored_supervisor_locks_beam",
        testonly = True,
        srcs = ["src/mirrored_supervisor_locks.erl"],
        outs = ["test/mirrored_supervisor_locks.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_mnesia_sync_beam",
        testonly = True,
        srcs = ["src/mnesia_sync.erl"],
        outs = ["test/mnesia_sync.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_pmon_beam",
        testonly = True,
        srcs = ["src/pmon.erl"],
        outs = ["test/pmon.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_priority_queue_beam",
        testonly = True,
        srcs = ["src/priority_queue.erl"],
        outs = ["test/priority_queue.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_amqp_connection_beam",
        testonly = True,
        srcs = ["src/rabbit_amqp_connection.erl"],
        outs = ["test/rabbit_amqp_connection.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_amqqueue_common_beam",
        testonly = True,
        srcs = ["src/rabbit_amqqueue_common.erl"],
        outs = ["test/rabbit_amqqueue_common.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_auth_backend_dummy_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_backend_dummy.erl"],
        outs = ["test/rabbit_auth_backend_dummy.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        beam = ["ebin/rabbit_authn_backend.beam", "ebin/rabbit_authz_backend.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_auth_mechanism_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_mechanism.erl"],
        outs = ["test/rabbit_auth_mechanism.beam"],
        beam = ["ebin/rabbit_registry_class.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_authn_backend_beam",
        testonly = True,
        srcs = ["src/rabbit_authn_backend.erl"],
        outs = ["test/rabbit_authn_backend.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_authz_backend_beam",
        testonly = True,
        srcs = ["src/rabbit_authz_backend.erl"],
        outs = ["test/rabbit_authz_backend.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_basic_common_beam",
        testonly = True,
        srcs = ["src/rabbit_basic_common.erl"],
        outs = ["test/rabbit_basic_common.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_binary_generator_beam",
        testonly = True,
        srcs = ["src/rabbit_binary_generator.erl"],
        outs = ["test/rabbit_binary_generator.beam"],
        hdrs = ["include/rabbit.hrl", "include/rabbit_framing.hrl", "include/resource.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_binary_parser_beam",
        testonly = True,
        srcs = ["src/rabbit_binary_parser.erl"],
        outs = ["test/rabbit_binary_parser.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_cert_info_beam",
        testonly = True,
        srcs = ["src/rabbit_cert_info.erl"],
        outs = ["test/rabbit_cert_info.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_channel_common_beam",
        testonly = True,
        srcs = ["src/rabbit_channel_common.erl"],
        outs = ["test/rabbit_channel_common.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_command_assembler_beam",
        testonly = True,
        srcs = ["src/rabbit_command_assembler.erl"],
        outs = ["test/rabbit_command_assembler.beam"],
        hdrs = ["include/rabbit.hrl", "include/rabbit_framing.hrl", "include/resource.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_control_misc_beam",
        testonly = True,
        srcs = ["src/rabbit_control_misc.erl"],
        outs = ["test/rabbit_control_misc.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_core_metrics_beam",
        testonly = True,
        srcs = ["src/rabbit_core_metrics.erl"],
        outs = ["test/rabbit_core_metrics.beam"],
        hdrs = ["include/rabbit_core_metrics.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_data_coercion_beam",
        testonly = True,
        srcs = ["src/rabbit_data_coercion.erl"],
        outs = ["test/rabbit_data_coercion.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_date_time_beam",
        testonly = True,
        srcs = ["src/rabbit_date_time.erl"],
        outs = ["test/rabbit_date_time.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_env_beam",
        testonly = True,
        srcs = ["src/rabbit_env.erl"],
        outs = ["test/rabbit_env.beam"],
        hdrs = ["include/logging.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_error_logger_handler_beam",
        testonly = True,
        srcs = ["src/rabbit_error_logger_handler.erl"],
        outs = ["test/rabbit_error_logger_handler.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_event_beam",
        testonly = True,
        srcs = ["src/rabbit_event.erl"],
        outs = ["test/rabbit_event.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_exchange_type_beam",
        testonly = True,
        srcs = ["src/rabbit_exchange_type.erl"],
        outs = ["test/rabbit_exchange_type.beam"],
        beam = ["ebin/rabbit_registry_class.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_heartbeat_beam",
        testonly = True,
        srcs = ["src/rabbit_heartbeat.erl"],
        outs = ["test/rabbit_heartbeat.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_http_util_beam",
        testonly = True,
        srcs = ["src/rabbit_http_util.erl"],
        outs = ["test/rabbit_http_util.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_json_beam",
        testonly = True,
        srcs = ["src/rabbit_json.erl"],
        outs = ["test/rabbit_json.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_log_beam",
        testonly = True,
        srcs = ["src/rabbit_log.erl"],
        outs = ["test/rabbit_log.beam"],
        hdrs = ["include/logging.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_misc_beam",
        testonly = True,
        srcs = ["src/rabbit_misc.erl"],
        outs = ["test/rabbit_misc.beam"],
        hdrs = ["include/rabbit.hrl", "include/rabbit_framing.hrl", "include/rabbit_misc.hrl", "include/resource.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_msg_store_index_beam",
        testonly = True,
        srcs = ["src/rabbit_msg_store_index.erl"],
        outs = ["test/rabbit_msg_store_index.beam"],
        hdrs = ["include/rabbit.hrl", "include/rabbit_msg_store.hrl", "include/resource.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_net_beam",
        testonly = True,
        srcs = ["src/rabbit_net.erl"],
        outs = ["test/rabbit_net.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_nodes_common_beam",
        testonly = True,
        srcs = ["src/rabbit_nodes_common.erl"],
        outs = ["test/rabbit_nodes_common.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_numerical_beam",
        testonly = True,
        srcs = ["src/rabbit_numerical.erl"],
        outs = ["test/rabbit_numerical.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_password_hashing_beam",
        testonly = True,
        srcs = ["src/rabbit_password_hashing.erl"],
        outs = ["test/rabbit_password_hashing.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_pbe_beam",
        testonly = True,
        srcs = ["src/rabbit_pbe.erl"],
        outs = ["test/rabbit_pbe.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_peer_discovery_backend_beam",
        testonly = True,
        srcs = ["src/rabbit_peer_discovery_backend.erl"],
        outs = ["test/rabbit_peer_discovery_backend.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_policy_validator_beam",
        testonly = True,
        srcs = ["src/rabbit_policy_validator.erl"],
        outs = ["test/rabbit_policy_validator.beam"],
        beam = ["ebin/rabbit_registry_class.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_queue_collector_beam",
        testonly = True,
        srcs = ["src/rabbit_queue_collector.erl"],
        outs = ["test/rabbit_queue_collector.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_registry_beam",
        testonly = True,
        srcs = ["src/rabbit_registry.erl"],
        outs = ["test/rabbit_registry.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_registry_class_beam",
        testonly = True,
        srcs = ["src/rabbit_registry_class.erl"],
        outs = ["test/rabbit_registry_class.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_resource_monitor_misc_beam",
        testonly = True,
        srcs = ["src/rabbit_resource_monitor_misc.erl"],
        outs = ["test/rabbit_resource_monitor_misc.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_runtime_beam",
        testonly = True,
        srcs = ["src/rabbit_runtime.erl"],
        outs = ["test/rabbit_runtime.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_runtime_parameter_beam",
        testonly = True,
        srcs = ["src/rabbit_runtime_parameter.erl"],
        outs = ["test/rabbit_runtime_parameter.beam"],
        beam = ["ebin/rabbit_registry_class.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_semver_beam",
        testonly = True,
        srcs = ["src/rabbit_semver.erl"],
        outs = ["test/rabbit_semver.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_semver_parser_beam",
        testonly = True,
        srcs = ["src/rabbit_semver_parser.erl"],
        outs = ["test/rabbit_semver_parser.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_ssl_options_beam",
        testonly = True,
        srcs = ["src/rabbit_ssl_options.erl"],
        outs = ["test/rabbit_ssl_options.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_types_beam",
        testonly = True,
        srcs = ["src/rabbit_types.erl"],
        outs = ["test/rabbit_types.beam"],
        hdrs = ["include/rabbit.hrl", "include/resource.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_writer_beam",
        testonly = True,
        srcs = ["src/rabbit_writer.erl"],
        outs = ["test/rabbit_writer.beam"],
        hdrs = ["include/rabbit.hrl", "include/rabbit_framing.hrl", "include/resource.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_supervisor2_beam",
        testonly = True,
        srcs = ["src/supervisor2.erl"],
        outs = ["test/supervisor2.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_vm_memory_monitor_beam",
        testonly = True,
        srcs = ["src/vm_memory_monitor.erl"],
        outs = ["test/vm_memory_monitor.beam"],
        hdrs = ["include/rabbit_memory.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_worker_pool_beam",
        testonly = True,
        srcs = ["src/worker_pool.erl"],
        outs = ["test/worker_pool.beam"],
        beam = ["ebin/gen_server2.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_worker_pool_sup_beam",
        testonly = True,
        srcs = ["src/worker_pool_sup.erl"],
        outs = ["test/worker_pool_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_worker_pool_worker_beam",
        testonly = True,
        srcs = ["src/worker_pool_worker.erl"],
        outs = ["test/worker_pool_worker.beam"],
        beam = ["ebin/gen_server2.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_framing_amqp_0_9_1_beam",
        testonly = True,
        srcs = ["src/rabbit_framing_amqp_0_9_1.erl"],
        outs = ["test/rabbit_framing_amqp_0_9_1.beam"],
        hdrs = ["include/rabbit_framing.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_framing_amqp_0_8_beam",
        testonly = True,
        srcs = ["src/rabbit_framing_amqp_0_8.erl"],
        outs = ["test/rabbit_framing_amqp_0_8.beam"],
        hdrs = ["include/rabbit_framing.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/app_utils.beam", "test/code_version.beam", "test/credit_flow.beam", "test/delegate.beam", "test/delegate_sup.beam", "test/file_handle_cache.beam", "test/file_handle_cache_stats.beam", "test/gen_server2.beam", "test/mirrored_supervisor_locks.beam", "test/mnesia_sync.beam", "test/pmon.beam", "test/priority_queue.beam", "test/rabbit_amqp_connection.beam", "test/rabbit_amqqueue_common.beam", "test/rabbit_auth_backend_dummy.beam", "test/rabbit_auth_mechanism.beam", "test/rabbit_authn_backend.beam", "test/rabbit_authz_backend.beam", "test/rabbit_basic_common.beam", "test/rabbit_binary_generator.beam", "test/rabbit_binary_parser.beam", "test/rabbit_cert_info.beam", "test/rabbit_channel_common.beam", "test/rabbit_command_assembler.beam", "test/rabbit_control_misc.beam", "test/rabbit_core_metrics.beam", "test/rabbit_data_coercion.beam", "test/rabbit_date_time.beam", "test/rabbit_env.beam", "test/rabbit_error_logger_handler.beam", "test/rabbit_event.beam", "test/rabbit_exchange_type.beam", "test/rabbit_framing_amqp_0_8.beam", "test/rabbit_framing_amqp_0_9_1.beam", "test/rabbit_heartbeat.beam", "test/rabbit_http_util.beam", "test/rabbit_json.beam", "test/rabbit_log.beam", "test/rabbit_misc.beam", "test/rabbit_msg_store_index.beam", "test/rabbit_net.beam", "test/rabbit_nodes_common.beam", "test/rabbit_numerical.beam", "test/rabbit_password_hashing.beam", "test/rabbit_pbe.beam", "test/rabbit_peer_discovery_backend.beam", "test/rabbit_policy_validator.beam", "test/rabbit_queue_collector.beam", "test/rabbit_registry.beam", "test/rabbit_registry_class.beam", "test/rabbit_resource_monitor_misc.beam", "test/rabbit_runtime.beam", "test/rabbit_runtime_parameter.beam", "test/rabbit_semver.beam", "test/rabbit_semver_parser.beam", "test/rabbit_ssl_options.beam", "test/rabbit_types.beam", "test/rabbit_writer.beam", "test/supervisor2.beam", "test/vm_memory_monitor.beam", "test/worker_pool.beam", "test/worker_pool_sup.beam", "test/worker_pool_worker.beam"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/logging.hrl", "include/rabbit.hrl", "include/rabbit_core_metrics.hrl", "include/rabbit_framing.hrl", "include/rabbit_memory.hrl", "include/rabbit_misc.hrl", "include/rabbit_msg_store.hrl", "include/resource.hrl", "src/app_utils.erl", "src/code_version.erl", "src/credit_flow.erl", "src/delegate.erl", "src/delegate_sup.erl", "src/file_handle_cache.erl", "src/file_handle_cache_stats.erl", "src/gen_server2.erl", "src/mirrored_supervisor_locks.erl", "src/mnesia_sync.erl", "src/pmon.erl", "src/priority_queue.erl", "src/rabbit_amqp_connection.erl", "src/rabbit_amqqueue_common.erl", "src/rabbit_auth_backend_dummy.erl", "src/rabbit_auth_mechanism.erl", "src/rabbit_authn_backend.erl", "src/rabbit_authz_backend.erl", "src/rabbit_basic_common.erl", "src/rabbit_binary_generator.erl", "src/rabbit_binary_parser.erl", "src/rabbit_cert_info.erl", "src/rabbit_channel_common.erl", "src/rabbit_command_assembler.erl", "src/rabbit_control_misc.erl", "src/rabbit_core_metrics.erl", "src/rabbit_data_coercion.erl", "src/rabbit_date_time.erl", "src/rabbit_env.erl", "src/rabbit_error_logger_handler.erl", "src/rabbit_event.erl", "src/rabbit_exchange_type.erl", "src/rabbit_framing_amqp_0_8.erl", "src/rabbit_framing_amqp_0_9_1.erl", "src/rabbit_heartbeat.erl", "src/rabbit_http_util.erl", "src/rabbit_json.erl", "src/rabbit_log.erl", "src/rabbit_misc.erl", "src/rabbit_msg_store_index.erl", "src/rabbit_net.erl", "src/rabbit_nodes_common.erl", "src/rabbit_numerical.erl", "src/rabbit_password_hashing.erl", "src/rabbit_pbe.erl", "src/rabbit_peer_discovery_backend.erl", "src/rabbit_policy_validator.erl", "src/rabbit_queue_collector.erl", "src/rabbit_registry.erl", "src/rabbit_registry_class.erl", "src/rabbit_resource_monitor_misc.erl", "src/rabbit_runtime.erl", "src/rabbit_runtime_parameter.erl", "src/rabbit_semver.erl", "src/rabbit_semver_parser.erl", "src/rabbit_ssl_options.erl", "src/rabbit_types.erl", "src/rabbit_writer.erl", "src/supervisor2.erl", "src/vm_memory_monitor.erl", "src/worker_pool.erl", "src/worker_pool_sup.erl", "src/worker_pool_worker.erl"],
    )
