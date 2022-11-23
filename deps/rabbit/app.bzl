load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/amqqueue.beam", "ebin/background_gc.beam", "ebin/code_server_cache.beam", "ebin/gatherer.beam", "ebin/gm.beam", "ebin/internal_user.beam", "ebin/lqueue.beam", "ebin/mirrored_supervisor.beam", "ebin/mirrored_supervisor_sups.beam", "ebin/pg_local.beam", "ebin/pid_recomposition.beam", "ebin/rabbit.beam", "ebin/rabbit_access_control.beam", "ebin/rabbit_alarm.beam", "ebin/rabbit_amqqueue.beam", "ebin/rabbit_amqqueue_process.beam", "ebin/rabbit_amqqueue_sup.beam", "ebin/rabbit_amqqueue_sup_sup.beam", "ebin/rabbit_auth_backend_internal.beam", "ebin/rabbit_auth_mechanism_amqplain.beam", "ebin/rabbit_auth_mechanism_cr_demo.beam", "ebin/rabbit_auth_mechanism_plain.beam", "ebin/rabbit_autoheal.beam", "ebin/rabbit_backing_queue.beam", "ebin/rabbit_basic.beam", "ebin/rabbit_binding.beam", "ebin/rabbit_boot_steps.beam", "ebin/rabbit_channel.beam", "ebin/rabbit_channel_interceptor.beam", "ebin/rabbit_channel_sup.beam", "ebin/rabbit_channel_sup_sup.beam", "ebin/rabbit_channel_tracking.beam", "ebin/rabbit_channel_tracking_handler.beam", "ebin/rabbit_classic_queue.beam", "ebin/rabbit_classic_queue_index_v2.beam", "ebin/rabbit_classic_queue_store_v2.beam", "ebin/rabbit_client_sup.beam", "ebin/rabbit_config.beam", "ebin/rabbit_confirms.beam", "ebin/rabbit_connection_helper_sup.beam", "ebin/rabbit_connection_sup.beam", "ebin/rabbit_connection_tracking.beam", "ebin/rabbit_connection_tracking_handler.beam", "ebin/rabbit_control_pbe.beam", "ebin/rabbit_core_ff.beam", "ebin/rabbit_core_metrics_gc.beam", "ebin/rabbit_credential_validation.beam", "ebin/rabbit_credential_validator.beam", "ebin/rabbit_credential_validator_accept_everything.beam", "ebin/rabbit_credential_validator_min_password_length.beam", "ebin/rabbit_credential_validator_password_regexp.beam", "ebin/rabbit_dead_letter.beam", "ebin/rabbit_definitions.beam", "ebin/rabbit_definitions_hashing.beam", "ebin/rabbit_definitions_import_https.beam", "ebin/rabbit_definitions_import_local_filesystem.beam", "ebin/rabbit_diagnostics.beam", "ebin/rabbit_direct.beam", "ebin/rabbit_direct_reply_to.beam", "ebin/rabbit_disk_monitor.beam", "ebin/rabbit_epmd_monitor.beam", "ebin/rabbit_event_consumer.beam", "ebin/rabbit_exchange.beam", "ebin/rabbit_exchange_decorator.beam", "ebin/rabbit_exchange_parameters.beam", "ebin/rabbit_exchange_type_direct.beam", "ebin/rabbit_exchange_type_fanout.beam", "ebin/rabbit_exchange_type_headers.beam", "ebin/rabbit_exchange_type_invalid.beam", "ebin/rabbit_exchange_type_topic.beam", "ebin/rabbit_feature_flags.beam", "ebin/rabbit_ff_controller.beam", "ebin/rabbit_ff_extra.beam", "ebin/rabbit_ff_registry.beam", "ebin/rabbit_ff_registry_factory.beam", "ebin/rabbit_fhc_helpers.beam", "ebin/rabbit_fifo.beam", "ebin/rabbit_fifo_client.beam", "ebin/rabbit_fifo_dlx.beam", "ebin/rabbit_fifo_dlx_client.beam", "ebin/rabbit_fifo_dlx_sup.beam", "ebin/rabbit_fifo_dlx_worker.beam", "ebin/rabbit_fifo_index.beam", "ebin/rabbit_fifo_v0.beam", "ebin/rabbit_fifo_v1.beam", "ebin/rabbit_file.beam", "ebin/rabbit_framing.beam", "ebin/rabbit_global_counters.beam", "ebin/rabbit_guid.beam", "ebin/rabbit_health_check.beam", "ebin/rabbit_limiter.beam", "ebin/rabbit_log_channel.beam", "ebin/rabbit_log_connection.beam", "ebin/rabbit_log_feature_flags.beam", "ebin/rabbit_log_mirroring.beam", "ebin/rabbit_log_prelaunch.beam", "ebin/rabbit_log_queue.beam", "ebin/rabbit_log_tail.beam", "ebin/rabbit_logger_exchange_h.beam", "ebin/rabbit_looking_glass.beam", "ebin/rabbit_maintenance.beam", "ebin/rabbit_memory_monitor.beam", "ebin/rabbit_metrics.beam", "ebin/rabbit_mirror_queue_coordinator.beam", "ebin/rabbit_mirror_queue_master.beam", "ebin/rabbit_mirror_queue_misc.beam", "ebin/rabbit_mirror_queue_mode.beam", "ebin/rabbit_mirror_queue_mode_all.beam", "ebin/rabbit_mirror_queue_mode_exactly.beam", "ebin/rabbit_mirror_queue_mode_nodes.beam", "ebin/rabbit_mirror_queue_slave.beam", "ebin/rabbit_mirror_queue_sync.beam", "ebin/rabbit_mnesia.beam", "ebin/rabbit_mnesia_rename.beam", "ebin/rabbit_msg_file.beam", "ebin/rabbit_msg_record.beam", "ebin/rabbit_msg_store.beam", "ebin/rabbit_msg_store_ets_index.beam", "ebin/rabbit_msg_store_gc.beam", "ebin/rabbit_networking.beam", "ebin/rabbit_networking_store.beam", "ebin/rabbit_node_monitor.beam", "ebin/rabbit_nodes.beam", "ebin/rabbit_observer_cli.beam", "ebin/rabbit_observer_cli_classic_queues.beam", "ebin/rabbit_osiris_metrics.beam", "ebin/rabbit_parameter_validation.beam", "ebin/rabbit_password.beam", "ebin/rabbit_password_hashing_md5.beam", "ebin/rabbit_password_hashing_sha256.beam", "ebin/rabbit_password_hashing_sha512.beam", "ebin/rabbit_peer_discovery.beam", "ebin/rabbit_peer_discovery_classic_config.beam", "ebin/rabbit_peer_discovery_dns.beam", "ebin/rabbit_plugins.beam", "ebin/rabbit_policies.beam", "ebin/rabbit_policy.beam", "ebin/rabbit_policy_merge_strategy.beam", "ebin/rabbit_prelaunch_cluster.beam", "ebin/rabbit_prelaunch_enabled_plugins_file.beam", "ebin/rabbit_prelaunch_feature_flags.beam", "ebin/rabbit_prelaunch_logging.beam", "ebin/rabbit_prequeue.beam", "ebin/rabbit_priority_queue.beam", "ebin/rabbit_queue_consumers.beam", "ebin/rabbit_queue_decorator.beam", "ebin/rabbit_queue_index.beam", "ebin/rabbit_queue_location.beam", "ebin/rabbit_queue_location_client_local.beam", "ebin/rabbit_queue_location_min_masters.beam", "ebin/rabbit_queue_location_random.beam", "ebin/rabbit_queue_location_validator.beam", "ebin/rabbit_queue_master_location_misc.beam", "ebin/rabbit_queue_master_locator.beam", "ebin/rabbit_queue_type.beam", "ebin/rabbit_queue_type_util.beam", "ebin/rabbit_quorum_memory_manager.beam", "ebin/rabbit_quorum_queue.beam", "ebin/rabbit_ra_registry.beam", "ebin/rabbit_ra_systems.beam", "ebin/rabbit_reader.beam", "ebin/rabbit_recovery_terms.beam", "ebin/rabbit_release_series.beam", "ebin/rabbit_restartable_sup.beam", "ebin/rabbit_router.beam", "ebin/rabbit_runtime_parameters.beam", "ebin/rabbit_ssl.beam", "ebin/rabbit_stream_coordinator.beam", "ebin/rabbit_stream_queue.beam", "ebin/rabbit_stream_sac_coordinator.beam", "ebin/rabbit_sup.beam", "ebin/rabbit_sysmon_handler.beam", "ebin/rabbit_sysmon_minder.beam", "ebin/rabbit_table.beam", "ebin/rabbit_time_travel_dbg.beam", "ebin/rabbit_trace.beam", "ebin/rabbit_tracking.beam", "ebin/rabbit_tracking_store.beam", "ebin/rabbit_upgrade_preparation.beam", "ebin/rabbit_variable_queue.beam", "ebin/rabbit_version.beam", "ebin/rabbit_vhost.beam", "ebin/rabbit_vhost_limit.beam", "ebin/rabbit_vhost_msg_store.beam", "ebin/rabbit_vhost_process.beam", "ebin/rabbit_vhost_sup.beam", "ebin/rabbit_vhost_sup_sup.beam", "ebin/rabbit_vhost_sup_wrapper.beam", "ebin/rabbit_vm.beam", "ebin/supervised_lifecycle.beam", "ebin/tcp_listener.beam", "ebin/tcp_listener_sup.beam", "ebin/term_to_binary_compat.beam", "ebin/vhost.beam"],
    )
    erlang_bytecode(
        name = "ebin_amqqueue_beam",
        srcs = ["src/amqqueue.erl"],
        outs = ["ebin/amqqueue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_background_gc_beam",
        srcs = ["src/background_gc.erl"],
        outs = ["ebin/background_gc.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_code_server_cache_beam",
        srcs = ["src/code_server_cache.erl"],
        outs = ["ebin/code_server_cache.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_gatherer_beam",
        srcs = ["src/gatherer.erl"],
        outs = ["ebin/gatherer.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_gm_beam",
        srcs = ["src/gm.erl"],
        outs = ["ebin/gm.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_internal_user_beam",
        srcs = ["src/internal_user.erl"],
        outs = ["ebin/internal_user.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_lqueue_beam",
        srcs = ["src/lqueue.erl"],
        outs = ["ebin/lqueue.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_mirrored_supervisor_beam",
        srcs = ["src/mirrored_supervisor.erl"],
        outs = ["ebin/mirrored_supervisor.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_mirrored_supervisor_sups_beam",
        srcs = ["src/mirrored_supervisor_sups.erl"],
        outs = ["ebin/mirrored_supervisor_sups.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_pg_local_beam",
        srcs = ["src/pg_local.erl"],
        outs = ["ebin/pg_local.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_pid_recomposition_beam",
        srcs = ["src/pid_recomposition.erl"],
        outs = ["ebin/pid_recomposition.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_access_control_beam",
        srcs = ["src/rabbit_access_control.erl"],
        outs = ["ebin/rabbit_access_control.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_alarm_beam",
        srcs = ["src/rabbit_alarm.erl"],
        outs = ["ebin/rabbit_alarm.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqqueue_beam",
        srcs = ["src/rabbit_amqqueue.erl"],
        outs = ["ebin/rabbit_amqqueue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqqueue_process_beam",
        srcs = ["src/rabbit_amqqueue_process.erl"],
        outs = ["ebin/rabbit_amqqueue_process.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqqueue_sup_beam",
        srcs = ["src/rabbit_amqqueue_sup.erl"],
        outs = ["ebin/rabbit_amqqueue_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_amqqueue_sup_sup_beam",
        srcs = ["src/rabbit_amqqueue_sup_sup.erl"],
        outs = ["ebin/rabbit_amqqueue_sup_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_backend_internal_beam",
        srcs = ["src/rabbit_auth_backend_internal.erl"],
        outs = ["ebin/rabbit_auth_backend_internal.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_mechanism_amqplain_beam",
        srcs = ["src/rabbit_auth_mechanism_amqplain.erl"],
        outs = ["ebin/rabbit_auth_mechanism_amqplain.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_mechanism_cr_demo_beam",
        srcs = ["src/rabbit_auth_mechanism_cr_demo.erl"],
        outs = ["ebin/rabbit_auth_mechanism_cr_demo.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_mechanism_plain_beam",
        srcs = ["src/rabbit_auth_mechanism_plain.erl"],
        outs = ["ebin/rabbit_auth_mechanism_plain.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_autoheal_beam",
        srcs = ["src/rabbit_autoheal.erl"],
        outs = ["ebin/rabbit_autoheal.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_backing_queue_beam",
        srcs = ["src/rabbit_backing_queue.erl"],
        outs = ["ebin/rabbit_backing_queue.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_basic_beam",
        srcs = ["src/rabbit_basic.erl"],
        outs = ["ebin/rabbit_basic.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_beam",
        srcs = ["src/rabbit.erl"],
        outs = ["ebin/rabbit.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_binding_beam",
        srcs = ["src/rabbit_binding.erl"],
        outs = ["ebin/rabbit_binding.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_boot_steps_beam",
        srcs = ["src/rabbit_boot_steps.erl"],
        outs = ["ebin/rabbit_boot_steps.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_channel_beam",
        srcs = ["src/rabbit_channel.erl"],
        outs = ["ebin/rabbit_channel.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_channel_interceptor_beam",
        srcs = ["src/rabbit_channel_interceptor.erl"],
        outs = ["ebin/rabbit_channel_interceptor.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_channel_sup_beam",
        srcs = ["src/rabbit_channel_sup.erl"],
        outs = ["ebin/rabbit_channel_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_channel_sup_sup_beam",
        srcs = ["src/rabbit_channel_sup_sup.erl"],
        outs = ["ebin/rabbit_channel_sup_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_channel_tracking_beam",
        srcs = ["src/rabbit_channel_tracking.erl"],
        outs = ["ebin/rabbit_channel_tracking.beam"],
        beam = ["ebin/rabbit_tracking.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_channel_tracking_handler_beam",
        srcs = ["src/rabbit_channel_tracking_handler.erl"],
        outs = ["ebin/rabbit_channel_tracking_handler.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_classic_queue_beam",
        srcs = ["src/rabbit_classic_queue.erl"],
        outs = ["ebin/rabbit_classic_queue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_queue_type.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_classic_queue_index_v2_beam",
        srcs = ["src/rabbit_classic_queue_index_v2.erl"],
        outs = ["ebin/rabbit_classic_queue_index_v2.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_classic_queue_store_v2_beam",
        srcs = ["src/rabbit_classic_queue_store_v2.erl"],
        outs = ["ebin/rabbit_classic_queue_store_v2.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_client_sup_beam",
        srcs = ["src/rabbit_client_sup.erl"],
        outs = ["ebin/rabbit_client_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_config_beam",
        srcs = ["src/rabbit_config.erl"],
        outs = ["ebin/rabbit_config.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_confirms_beam",
        srcs = ["src/rabbit_confirms.erl"],
        outs = ["ebin/rabbit_confirms.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_connection_helper_sup_beam",
        srcs = ["src/rabbit_connection_helper_sup.erl"],
        outs = ["ebin/rabbit_connection_helper_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_connection_sup_beam",
        srcs = ["src/rabbit_connection_sup.erl"],
        outs = ["ebin/rabbit_connection_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@ranch//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_connection_tracking_beam",
        srcs = ["src/rabbit_connection_tracking.erl"],
        outs = ["ebin/rabbit_connection_tracking.beam"],
        beam = ["ebin/rabbit_tracking.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_connection_tracking_handler_beam",
        srcs = ["src/rabbit_connection_tracking_handler.erl"],
        outs = ["ebin/rabbit_connection_tracking_handler.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_control_pbe_beam",
        srcs = ["src/rabbit_control_pbe.erl"],
        outs = ["ebin/rabbit_control_pbe.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_core_ff_beam",
        srcs = ["src/rabbit_core_ff.erl"],
        outs = ["ebin/rabbit_core_ff.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_core_metrics_gc_beam",
        srcs = ["src/rabbit_core_metrics_gc.erl"],
        outs = ["ebin/rabbit_core_metrics_gc.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_credential_validation_beam",
        srcs = ["src/rabbit_credential_validation.erl"],
        outs = ["ebin/rabbit_credential_validation.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_credential_validator_accept_everything_beam",
        srcs = ["src/rabbit_credential_validator_accept_everything.erl"],
        outs = ["ebin/rabbit_credential_validator_accept_everything.beam"],
        beam = ["ebin/rabbit_credential_validator.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_credential_validator_beam",
        srcs = ["src/rabbit_credential_validator.erl"],
        outs = ["ebin/rabbit_credential_validator.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_credential_validator_min_password_length_beam",
        srcs = ["src/rabbit_credential_validator_min_password_length.erl"],
        outs = ["ebin/rabbit_credential_validator_min_password_length.beam"],
        beam = ["ebin/rabbit_credential_validator.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_credential_validator_password_regexp_beam",
        srcs = ["src/rabbit_credential_validator_password_regexp.erl"],
        outs = ["ebin/rabbit_credential_validator_password_regexp.beam"],
        beam = ["ebin/rabbit_credential_validator.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_dead_letter_beam",
        srcs = ["src/rabbit_dead_letter.erl"],
        outs = ["ebin/rabbit_dead_letter.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_definitions_beam",
        srcs = ["src/rabbit_definitions.erl"],
        outs = ["ebin/rabbit_definitions.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_definitions_hashing_beam",
        srcs = ["src/rabbit_definitions_hashing.erl"],
        outs = ["ebin/rabbit_definitions_hashing.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_definitions_import_https_beam",
        srcs = ["src/rabbit_definitions_import_https.erl"],
        outs = ["ebin/rabbit_definitions_import_https.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_definitions_import_local_filesystem_beam",
        srcs = ["src/rabbit_definitions_import_local_filesystem.erl"],
        outs = ["ebin/rabbit_definitions_import_local_filesystem.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_diagnostics_beam",
        srcs = ["src/rabbit_diagnostics.erl"],
        outs = ["ebin/rabbit_diagnostics.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_direct_beam",
        srcs = ["src/rabbit_direct.erl"],
        outs = ["ebin/rabbit_direct.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_direct_reply_to_beam",
        srcs = ["src/rabbit_direct_reply_to.erl"],
        outs = ["ebin/rabbit_direct_reply_to.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_disk_monitor_beam",
        srcs = ["src/rabbit_disk_monitor.erl"],
        outs = ["ebin/rabbit_disk_monitor.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_epmd_monitor_beam",
        srcs = ["src/rabbit_epmd_monitor.erl"],
        outs = ["ebin/rabbit_epmd_monitor.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_event_consumer_beam",
        srcs = ["src/rabbit_event_consumer.erl"],
        outs = ["ebin/rabbit_event_consumer.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_exchange_beam",
        srcs = ["src/rabbit_exchange.erl"],
        outs = ["ebin/rabbit_exchange.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_exchange_decorator_beam",
        srcs = ["src/rabbit_exchange_decorator.erl"],
        outs = ["ebin/rabbit_exchange_decorator.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_exchange_parameters_beam",
        srcs = ["src/rabbit_exchange_parameters.erl"],
        outs = ["ebin/rabbit_exchange_parameters.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_exchange_type_direct_beam",
        srcs = ["src/rabbit_exchange_type_direct.erl"],
        outs = ["ebin/rabbit_exchange_type_direct.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_exchange_type_fanout_beam",
        srcs = ["src/rabbit_exchange_type_fanout.erl"],
        outs = ["ebin/rabbit_exchange_type_fanout.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_exchange_type_headers_beam",
        srcs = ["src/rabbit_exchange_type_headers.erl"],
        outs = ["ebin/rabbit_exchange_type_headers.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_exchange_type_invalid_beam",
        srcs = ["src/rabbit_exchange_type_invalid.erl"],
        outs = ["ebin/rabbit_exchange_type_invalid.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_exchange_type_topic_beam",
        srcs = ["src/rabbit_exchange_type_topic.erl"],
        outs = ["ebin/rabbit_exchange_type_topic.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_feature_flags_beam",
        srcs = ["src/rabbit_feature_flags.erl"],
        outs = ["ebin/rabbit_feature_flags.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_ff_controller_beam",
        srcs = ["src/rabbit_ff_controller.erl"],
        outs = ["ebin/rabbit_ff_controller.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_ff_extra_beam",
        srcs = ["src/rabbit_ff_extra.erl"],
        outs = ["ebin/rabbit_ff_extra.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["@stdout_formatter//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_ff_registry_beam",
        srcs = ["src/rabbit_ff_registry.erl"],
        outs = ["ebin/rabbit_ff_registry.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_ff_registry_factory_beam",
        srcs = ["src/rabbit_ff_registry_factory.erl"],
        outs = ["ebin/rabbit_ff_registry_factory.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_fhc_helpers_beam",
        srcs = ["src/rabbit_fhc_helpers.erl"],
        outs = ["ebin/rabbit_fhc_helpers.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_fifo_beam",
        srcs = ["src/rabbit_fifo.erl"],
        outs = ["ebin/rabbit_fifo.beam"],
        hdrs = ["src/rabbit_fifo.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@ra//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_fifo_client_beam",
        srcs = ["src/rabbit_fifo_client.erl"],
        outs = ["ebin/rabbit_fifo_client.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_fifo_dlx_beam",
        srcs = ["src/rabbit_fifo_dlx.erl"],
        outs = ["ebin/rabbit_fifo_dlx.beam"],
        hdrs = ["src/rabbit_fifo.hrl", "src/rabbit_fifo_dlx.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_fifo_dlx_client_beam",
        srcs = ["src/rabbit_fifo_dlx_client.erl"],
        outs = ["ebin/rabbit_fifo_dlx_client.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_fifo_dlx_sup_beam",
        srcs = ["src/rabbit_fifo_dlx_sup.erl"],
        outs = ["ebin/rabbit_fifo_dlx_sup.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_fifo_dlx_worker_beam",
        srcs = ["src/rabbit_fifo_dlx_worker.erl"],
        outs = ["ebin/rabbit_fifo_dlx_worker.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_fifo_index_beam",
        srcs = ["src/rabbit_fifo_index.erl"],
        outs = ["ebin/rabbit_fifo_index.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_fifo_v0_beam",
        srcs = ["src/rabbit_fifo_v0.erl"],
        outs = ["ebin/rabbit_fifo_v0.beam"],
        hdrs = ["src/rabbit_fifo_v0.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@ra//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_fifo_v1_beam",
        srcs = ["src/rabbit_fifo_v1.erl"],
        outs = ["ebin/rabbit_fifo_v1.beam"],
        hdrs = ["src/rabbit_fifo_v1.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@ra//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_file_beam",
        srcs = ["src/rabbit_file.erl"],
        outs = ["ebin/rabbit_file.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_framing_beam",
        srcs = ["src/rabbit_framing.erl"],
        outs = ["ebin/rabbit_framing.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_global_counters_beam",
        srcs = ["src/rabbit_global_counters.erl"],
        outs = ["ebin/rabbit_global_counters.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_guid_beam",
        srcs = ["src/rabbit_guid.erl"],
        outs = ["ebin/rabbit_guid.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_health_check_beam",
        srcs = ["src/rabbit_health_check.erl"],
        outs = ["ebin/rabbit_health_check.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_limiter_beam",
        srcs = ["src/rabbit_limiter.erl"],
        outs = ["ebin/rabbit_limiter.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_log_channel_beam",
        srcs = ["src/rabbit_log_channel.erl"],
        outs = ["ebin/rabbit_log_channel.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_log_connection_beam",
        srcs = ["src/rabbit_log_connection.erl"],
        outs = ["ebin/rabbit_log_connection.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_log_feature_flags_beam",
        srcs = ["src/rabbit_log_feature_flags.erl"],
        outs = ["ebin/rabbit_log_feature_flags.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_log_mirroring_beam",
        srcs = ["src/rabbit_log_mirroring.erl"],
        outs = ["ebin/rabbit_log_mirroring.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_log_prelaunch_beam",
        srcs = ["src/rabbit_log_prelaunch.erl"],
        outs = ["ebin/rabbit_log_prelaunch.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_log_queue_beam",
        srcs = ["src/rabbit_log_queue.erl"],
        outs = ["ebin/rabbit_log_queue.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_log_tail_beam",
        srcs = ["src/rabbit_log_tail.erl"],
        outs = ["ebin/rabbit_log_tail.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_logger_exchange_h_beam",
        srcs = ["src/rabbit_logger_exchange_h.erl"],
        outs = ["ebin/rabbit_logger_exchange_h.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_looking_glass_beam",
        srcs = ["src/rabbit_looking_glass.erl"],
        outs = ["ebin/rabbit_looking_glass.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_maintenance_beam",
        srcs = ["src/rabbit_maintenance.erl"],
        outs = ["ebin/rabbit_maintenance.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_memory_monitor_beam",
        srcs = ["src/rabbit_memory_monitor.erl"],
        outs = ["ebin/rabbit_memory_monitor.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_metrics_beam",
        srcs = ["src/rabbit_metrics.erl"],
        outs = ["ebin/rabbit_metrics.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mirror_queue_coordinator_beam",
        srcs = ["src/rabbit_mirror_queue_coordinator.erl"],
        outs = ["ebin/rabbit_mirror_queue_coordinator.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl", "include/gm_specs.hrl"],
        beam = ["ebin/gm.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mirror_queue_master_beam",
        srcs = ["src/rabbit_mirror_queue_master.erl"],
        outs = ["ebin/rabbit_mirror_queue_master.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_backing_queue.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mirror_queue_misc_beam",
        srcs = ["src/rabbit_mirror_queue_misc.erl"],
        outs = ["ebin/rabbit_mirror_queue_misc.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mirror_queue_mode_all_beam",
        srcs = ["src/rabbit_mirror_queue_mode_all.erl"],
        outs = ["ebin/rabbit_mirror_queue_mode_all.beam"],
        beam = ["ebin/rabbit_mirror_queue_mode.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mirror_queue_mode_beam",
        srcs = ["src/rabbit_mirror_queue_mode.erl"],
        outs = ["ebin/rabbit_mirror_queue_mode.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mirror_queue_mode_exactly_beam",
        srcs = ["src/rabbit_mirror_queue_mode_exactly.erl"],
        outs = ["ebin/rabbit_mirror_queue_mode_exactly.beam"],
        beam = ["ebin/rabbit_mirror_queue_mode.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mirror_queue_mode_nodes_beam",
        srcs = ["src/rabbit_mirror_queue_mode_nodes.erl"],
        outs = ["ebin/rabbit_mirror_queue_mode_nodes.beam"],
        beam = ["ebin/rabbit_mirror_queue_mode.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mirror_queue_slave_beam",
        srcs = ["src/rabbit_mirror_queue_slave.erl"],
        outs = ["ebin/rabbit_mirror_queue_slave.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl", "include/gm_specs.hrl"],
        beam = ["ebin/gm.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mirror_queue_sync_beam",
        srcs = ["src/rabbit_mirror_queue_sync.erl"],
        outs = ["ebin/rabbit_mirror_queue_sync.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_mnesia_beam",
        srcs = ["src/rabbit_mnesia.erl"],
        outs = ["ebin/rabbit_mnesia.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_mnesia_rename_beam",
        srcs = ["src/rabbit_mnesia_rename.erl"],
        outs = ["ebin/rabbit_mnesia_rename.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_msg_file_beam",
        srcs = ["src/rabbit_msg_file.erl"],
        outs = ["ebin/rabbit_msg_file.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_msg_record_beam",
        srcs = ["src/rabbit_msg_record.erl"],
        outs = ["ebin/rabbit_msg_record.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_msg_store_beam",
        srcs = ["src/rabbit_msg_store.erl"],
        outs = ["ebin/rabbit_msg_store.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_msg_store_ets_index_beam",
        srcs = ["src/rabbit_msg_store_ets_index.erl"],
        outs = ["ebin/rabbit_msg_store_ets_index.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_msg_store_gc_beam",
        srcs = ["src/rabbit_msg_store_gc.erl"],
        outs = ["ebin/rabbit_msg_store_gc.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_networking_beam",
        srcs = ["src/rabbit_networking.erl"],
        outs = ["ebin/rabbit_networking.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_networking_store_beam",
        srcs = ["src/rabbit_networking_store.erl"],
        outs = ["ebin/rabbit_networking_store.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_node_monitor_beam",
        srcs = ["src/rabbit_node_monitor.erl"],
        outs = ["ebin/rabbit_node_monitor.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_nodes_beam",
        srcs = ["src/rabbit_nodes.erl"],
        outs = ["ebin/rabbit_nodes.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_observer_cli_beam",
        srcs = ["src/rabbit_observer_cli.erl"],
        outs = ["ebin/rabbit_observer_cli.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_observer_cli_classic_queues_beam",
        srcs = ["src/rabbit_observer_cli_classic_queues.erl"],
        outs = ["ebin/rabbit_observer_cli_classic_queues.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_osiris_metrics_beam",
        srcs = ["src/rabbit_osiris_metrics.erl"],
        outs = ["ebin/rabbit_osiris_metrics.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_parameter_validation_beam",
        srcs = ["src/rabbit_parameter_validation.erl"],
        outs = ["ebin/rabbit_parameter_validation.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_password_beam",
        srcs = ["src/rabbit_password.erl"],
        outs = ["ebin/rabbit_password.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_password_hashing_md5_beam",
        srcs = ["src/rabbit_password_hashing_md5.erl"],
        outs = ["ebin/rabbit_password_hashing_md5.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_password_hashing_sha256_beam",
        srcs = ["src/rabbit_password_hashing_sha256.erl"],
        outs = ["ebin/rabbit_password_hashing_sha256.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_password_hashing_sha512_beam",
        srcs = ["src/rabbit_password_hashing_sha512.erl"],
        outs = ["ebin/rabbit_password_hashing_sha512.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_peer_discovery_beam",
        srcs = ["src/rabbit_peer_discovery.erl"],
        outs = ["ebin/rabbit_peer_discovery.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_peer_discovery_classic_config_beam",
        srcs = ["src/rabbit_peer_discovery_classic_config.erl"],
        outs = ["ebin/rabbit_peer_discovery_classic_config.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_peer_discovery_dns_beam",
        srcs = ["src/rabbit_peer_discovery_dns.erl"],
        outs = ["ebin/rabbit_peer_discovery_dns.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_plugins_beam",
        srcs = ["src/rabbit_plugins.erl"],
        outs = ["ebin/rabbit_plugins.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_policies_beam",
        srcs = ["src/rabbit_policies.erl"],
        outs = ["ebin/rabbit_policies.beam"],
        beam = ["ebin/rabbit_policy_merge_strategy.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_policy_beam",
        srcs = ["src/rabbit_policy.erl"],
        outs = ["ebin/rabbit_policy.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_policy_merge_strategy_beam",
        srcs = ["src/rabbit_policy_merge_strategy.erl"],
        outs = ["ebin/rabbit_policy_merge_strategy.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_prelaunch_cluster_beam",
        srcs = ["src/rabbit_prelaunch_cluster.erl"],
        outs = ["ebin/rabbit_prelaunch_cluster.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_prelaunch_enabled_plugins_file_beam",
        srcs = ["src/rabbit_prelaunch_enabled_plugins_file.erl"],
        outs = ["ebin/rabbit_prelaunch_enabled_plugins_file.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_prelaunch_feature_flags_beam",
        srcs = ["src/rabbit_prelaunch_feature_flags.erl"],
        outs = ["ebin/rabbit_prelaunch_feature_flags.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_prelaunch_logging_beam",
        srcs = ["src/rabbit_prelaunch_logging.erl"],
        outs = ["ebin/rabbit_prelaunch_logging.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_prequeue_beam",
        srcs = ["src/rabbit_prequeue.erl"],
        outs = ["ebin/rabbit_prequeue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_priority_queue_beam",
        srcs = ["src/rabbit_priority_queue.erl"],
        outs = ["ebin/rabbit_priority_queue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_backing_queue.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_queue_consumers_beam",
        srcs = ["src/rabbit_queue_consumers.erl"],
        outs = ["ebin/rabbit_queue_consumers.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_queue_decorator_beam",
        srcs = ["src/rabbit_queue_decorator.erl"],
        outs = ["ebin/rabbit_queue_decorator.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_queue_index_beam",
        srcs = ["src/rabbit_queue_index.erl"],
        outs = ["ebin/rabbit_queue_index.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_queue_location_beam",
        srcs = ["src/rabbit_queue_location.erl"],
        outs = ["ebin/rabbit_queue_location.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_queue_location_client_local_beam",
        srcs = ["src/rabbit_queue_location_client_local.erl"],
        outs = ["ebin/rabbit_queue_location_client_local.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_queue_master_locator.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_queue_location_min_masters_beam",
        srcs = ["src/rabbit_queue_location_min_masters.erl"],
        outs = ["ebin/rabbit_queue_location_min_masters.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_queue_master_locator.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_queue_location_random_beam",
        srcs = ["src/rabbit_queue_location_random.erl"],
        outs = ["ebin/rabbit_queue_location_random.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_queue_master_locator.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_queue_location_validator_beam",
        srcs = ["src/rabbit_queue_location_validator.erl"],
        outs = ["ebin/rabbit_queue_location_validator.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_queue_master_location_misc_beam",
        srcs = ["src/rabbit_queue_master_location_misc.erl"],
        outs = ["ebin/rabbit_queue_master_location_misc.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_queue_master_locator_beam",
        srcs = ["src/rabbit_queue_master_locator.erl"],
        outs = ["ebin/rabbit_queue_master_locator.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_queue_type_beam",
        srcs = ["src/rabbit_queue_type.erl"],
        outs = ["ebin/rabbit_queue_type.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_queue_type_util_beam",
        srcs = ["src/rabbit_queue_type_util.erl"],
        outs = ["ebin/rabbit_queue_type_util.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_quorum_memory_manager_beam",
        srcs = ["src/rabbit_quorum_memory_manager.erl"],
        outs = ["ebin/rabbit_quorum_memory_manager.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_quorum_queue_beam",
        srcs = ["src/rabbit_quorum_queue.erl"],
        outs = ["ebin/rabbit_quorum_queue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_queue_type.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_ra_registry_beam",
        srcs = ["src/rabbit_ra_registry.erl"],
        outs = ["ebin/rabbit_ra_registry.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_ra_systems_beam",
        srcs = ["src/rabbit_ra_systems.erl"],
        outs = ["ebin/rabbit_ra_systems.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_reader_beam",
        srcs = ["src/rabbit_reader.erl"],
        outs = ["ebin/rabbit_reader.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_recovery_terms_beam",
        srcs = ["src/rabbit_recovery_terms.erl"],
        outs = ["ebin/rabbit_recovery_terms.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_release_series_beam",
        srcs = ["src/rabbit_release_series.erl"],
        outs = ["ebin/rabbit_release_series.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_restartable_sup_beam",
        srcs = ["src/rabbit_restartable_sup.erl"],
        outs = ["ebin/rabbit_restartable_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_router_beam",
        srcs = ["src/rabbit_router.erl"],
        outs = ["ebin/rabbit_router.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_runtime_parameters_beam",
        srcs = ["src/rabbit_runtime_parameters.erl"],
        outs = ["ebin/rabbit_runtime_parameters.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_ssl_beam",
        srcs = ["src/rabbit_ssl.erl"],
        outs = ["ebin/rabbit_ssl.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_stream_coordinator_beam",
        srcs = ["src/rabbit_stream_coordinator.erl"],
        outs = ["ebin/rabbit_stream_coordinator.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl", "src/rabbit_stream_coordinator.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["@ra//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_stream_queue_beam",
        srcs = ["src/rabbit_stream_queue.erl"],
        outs = ["ebin/rabbit_stream_queue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_queue_type.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_stream_sac_coordinator_beam",
        srcs = ["src/rabbit_stream_sac_coordinator.erl"],
        outs = ["ebin/rabbit_stream_sac_coordinator.beam"],
        hdrs = ["src/rabbit_stream_sac_coordinator.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_sup_beam",
        srcs = ["src/rabbit_sup.erl"],
        outs = ["ebin/rabbit_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_sysmon_handler_beam",
        srcs = ["src/rabbit_sysmon_handler.erl"],
        outs = ["ebin/rabbit_sysmon_handler.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_sysmon_minder_beam",
        srcs = ["src/rabbit_sysmon_minder.erl"],
        outs = ["ebin/rabbit_sysmon_minder.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_table_beam",
        srcs = ["src/rabbit_table.erl"],
        outs = ["ebin/rabbit_table.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_time_travel_dbg_beam",
        srcs = ["src/rabbit_time_travel_dbg.erl"],
        outs = ["ebin/rabbit_time_travel_dbg.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_trace_beam",
        srcs = ["src/rabbit_trace.erl"],
        outs = ["ebin/rabbit_trace.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_tracking_beam",
        srcs = ["src/rabbit_tracking.erl"],
        outs = ["ebin/rabbit_tracking.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_tracking_store_beam",
        srcs = ["src/rabbit_tracking_store.erl"],
        outs = ["ebin/rabbit_tracking_store.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_upgrade_preparation_beam",
        srcs = ["src/rabbit_upgrade_preparation.erl"],
        outs = ["ebin/rabbit_upgrade_preparation.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_variable_queue_beam",
        srcs = ["src/rabbit_variable_queue.erl"],
        outs = ["ebin/rabbit_variable_queue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_backing_queue.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_version_beam",
        srcs = ["src/rabbit_version.erl"],
        outs = ["ebin/rabbit_version.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_vhost_beam",
        srcs = ["src/rabbit_vhost.erl"],
        outs = ["ebin/rabbit_vhost.beam"],
        hdrs = ["include/vhost.hrl", "include/vhost_v2.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_vhost_limit_beam",
        srcs = ["src/rabbit_vhost_limit.erl"],
        outs = ["ebin/rabbit_vhost_limit.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_vhost_msg_store_beam",
        srcs = ["src/rabbit_vhost_msg_store.erl"],
        outs = ["ebin/rabbit_vhost_msg_store.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_vhost_process_beam",
        srcs = ["src/rabbit_vhost_process.erl"],
        outs = ["ebin/rabbit_vhost_process.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_vhost_sup_beam",
        srcs = ["src/rabbit_vhost_sup.erl"],
        outs = ["ebin/rabbit_vhost_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_vhost_sup_sup_beam",
        srcs = ["src/rabbit_vhost_sup_sup.erl"],
        outs = ["ebin/rabbit_vhost_sup_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_vhost_sup_wrapper_beam",
        srcs = ["src/rabbit_vhost_sup_wrapper.erl"],
        outs = ["ebin/rabbit_vhost_sup_wrapper.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_vm_beam",
        srcs = ["src/rabbit_vm.erl"],
        outs = ["ebin/rabbit_vm.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_supervised_lifecycle_beam",
        srcs = ["src/supervised_lifecycle.erl"],
        outs = ["ebin/supervised_lifecycle.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_tcp_listener_beam",
        srcs = ["src/tcp_listener.erl"],
        outs = ["ebin/tcp_listener.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_tcp_listener_sup_beam",
        srcs = ["src/tcp_listener_sup.erl"],
        outs = ["ebin/tcp_listener_sup.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_term_to_binary_compat_beam",
        srcs = ["src/term_to_binary_compat.erl"],
        outs = ["ebin/term_to_binary_compat.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_vhost_beam",
        srcs = ["src/vhost.erl"],
        outs = ["ebin/vhost.beam"],
        hdrs = ["include/vhost.hrl", "include/vhost_v2.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_test_beam_files():
    erlang_bytecode(
        name = "test_amqqueue_beam",
        testonly = True,
        srcs = ["src/amqqueue.erl"],
        outs = ["test/amqqueue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_background_gc_beam",
        testonly = True,
        srcs = ["src/background_gc.erl"],
        outs = ["test/background_gc.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/amqqueue.beam", "test/background_gc.beam", "test/code_server_cache.beam", "test/gatherer.beam", "test/gm.beam", "test/internal_user.beam", "test/lqueue.beam", "test/mirrored_supervisor.beam", "test/mirrored_supervisor_sups.beam", "test/pg_local.beam", "test/pid_recomposition.beam", "test/rabbit.beam", "test/rabbit_access_control.beam", "test/rabbit_alarm.beam", "test/rabbit_amqqueue.beam", "test/rabbit_amqqueue_process.beam", "test/rabbit_amqqueue_sup.beam", "test/rabbit_amqqueue_sup_sup.beam", "test/rabbit_auth_backend_internal.beam", "test/rabbit_auth_mechanism_amqplain.beam", "test/rabbit_auth_mechanism_cr_demo.beam", "test/rabbit_auth_mechanism_plain.beam", "test/rabbit_autoheal.beam", "test/rabbit_backing_queue.beam", "test/rabbit_basic.beam", "test/rabbit_binding.beam", "test/rabbit_boot_steps.beam", "test/rabbit_channel.beam", "test/rabbit_channel_interceptor.beam", "test/rabbit_channel_sup.beam", "test/rabbit_channel_sup_sup.beam", "test/rabbit_channel_tracking.beam", "test/rabbit_channel_tracking_handler.beam", "test/rabbit_classic_queue.beam", "test/rabbit_classic_queue_index_v2.beam", "test/rabbit_classic_queue_store_v2.beam", "test/rabbit_client_sup.beam", "test/rabbit_config.beam", "test/rabbit_confirms.beam", "test/rabbit_connection_helper_sup.beam", "test/rabbit_connection_sup.beam", "test/rabbit_connection_tracking.beam", "test/rabbit_connection_tracking_handler.beam", "test/rabbit_control_pbe.beam", "test/rabbit_core_ff.beam", "test/rabbit_core_metrics_gc.beam", "test/rabbit_credential_validation.beam", "test/rabbit_credential_validator.beam", "test/rabbit_credential_validator_accept_everything.beam", "test/rabbit_credential_validator_min_password_length.beam", "test/rabbit_credential_validator_password_regexp.beam", "test/rabbit_dead_letter.beam", "test/rabbit_definitions.beam", "test/rabbit_definitions_hashing.beam", "test/rabbit_definitions_import_https.beam", "test/rabbit_definitions_import_local_filesystem.beam", "test/rabbit_diagnostics.beam", "test/rabbit_direct.beam", "test/rabbit_direct_reply_to.beam", "test/rabbit_disk_monitor.beam", "test/rabbit_epmd_monitor.beam", "test/rabbit_event_consumer.beam", "test/rabbit_exchange.beam", "test/rabbit_exchange_decorator.beam", "test/rabbit_exchange_parameters.beam", "test/rabbit_exchange_type_direct.beam", "test/rabbit_exchange_type_fanout.beam", "test/rabbit_exchange_type_headers.beam", "test/rabbit_exchange_type_invalid.beam", "test/rabbit_exchange_type_topic.beam", "test/rabbit_feature_flags.beam", "test/rabbit_ff_controller.beam", "test/rabbit_ff_extra.beam", "test/rabbit_ff_registry.beam", "test/rabbit_ff_registry_factory.beam", "test/rabbit_fhc_helpers.beam", "test/rabbit_fifo.beam", "test/rabbit_fifo_client.beam", "test/rabbit_fifo_dlx.beam", "test/rabbit_fifo_dlx_client.beam", "test/rabbit_fifo_dlx_sup.beam", "test/rabbit_fifo_dlx_worker.beam", "test/rabbit_fifo_index.beam", "test/rabbit_fifo_v0.beam", "test/rabbit_fifo_v1.beam", "test/rabbit_file.beam", "test/rabbit_framing.beam", "test/rabbit_global_counters.beam", "test/rabbit_guid.beam", "test/rabbit_health_check.beam", "test/rabbit_limiter.beam", "test/rabbit_log_channel.beam", "test/rabbit_log_connection.beam", "test/rabbit_log_feature_flags.beam", "test/rabbit_log_mirroring.beam", "test/rabbit_log_prelaunch.beam", "test/rabbit_log_queue.beam", "test/rabbit_log_tail.beam", "test/rabbit_logger_exchange_h.beam", "test/rabbit_looking_glass.beam", "test/rabbit_maintenance.beam", "test/rabbit_memory_monitor.beam", "test/rabbit_metrics.beam", "test/rabbit_mirror_queue_coordinator.beam", "test/rabbit_mirror_queue_master.beam", "test/rabbit_mirror_queue_misc.beam", "test/rabbit_mirror_queue_mode.beam", "test/rabbit_mirror_queue_mode_all.beam", "test/rabbit_mirror_queue_mode_exactly.beam", "test/rabbit_mirror_queue_mode_nodes.beam", "test/rabbit_mirror_queue_slave.beam", "test/rabbit_mirror_queue_sync.beam", "test/rabbit_mnesia.beam", "test/rabbit_mnesia_rename.beam", "test/rabbit_msg_file.beam", "test/rabbit_msg_record.beam", "test/rabbit_msg_store.beam", "test/rabbit_msg_store_ets_index.beam", "test/rabbit_msg_store_gc.beam", "test/rabbit_networking.beam", "test/rabbit_networking_store.beam", "test/rabbit_node_monitor.beam", "test/rabbit_nodes.beam", "test/rabbit_observer_cli.beam", "test/rabbit_observer_cli_classic_queues.beam", "test/rabbit_osiris_metrics.beam", "test/rabbit_parameter_validation.beam", "test/rabbit_password.beam", "test/rabbit_password_hashing_md5.beam", "test/rabbit_password_hashing_sha256.beam", "test/rabbit_password_hashing_sha512.beam", "test/rabbit_peer_discovery.beam", "test/rabbit_peer_discovery_classic_config.beam", "test/rabbit_peer_discovery_dns.beam", "test/rabbit_plugins.beam", "test/rabbit_policies.beam", "test/rabbit_policy.beam", "test/rabbit_policy_merge_strategy.beam", "test/rabbit_prelaunch_cluster.beam", "test/rabbit_prelaunch_enabled_plugins_file.beam", "test/rabbit_prelaunch_feature_flags.beam", "test/rabbit_prelaunch_logging.beam", "test/rabbit_prequeue.beam", "test/rabbit_priority_queue.beam", "test/rabbit_queue_consumers.beam", "test/rabbit_queue_decorator.beam", "test/rabbit_queue_index.beam", "test/rabbit_queue_location.beam", "test/rabbit_queue_location_client_local.beam", "test/rabbit_queue_location_min_masters.beam", "test/rabbit_queue_location_random.beam", "test/rabbit_queue_location_validator.beam", "test/rabbit_queue_master_location_misc.beam", "test/rabbit_queue_master_locator.beam", "test/rabbit_queue_type.beam", "test/rabbit_queue_type_util.beam", "test/rabbit_quorum_memory_manager.beam", "test/rabbit_quorum_queue.beam", "test/rabbit_ra_registry.beam", "test/rabbit_ra_systems.beam", "test/rabbit_reader.beam", "test/rabbit_recovery_terms.beam", "test/rabbit_release_series.beam", "test/rabbit_restartable_sup.beam", "test/rabbit_router.beam", "test/rabbit_runtime_parameters.beam", "test/rabbit_ssl.beam", "test/rabbit_stream_coordinator.beam", "test/rabbit_stream_queue.beam", "test/rabbit_stream_sac_coordinator.beam", "test/rabbit_sup.beam", "test/rabbit_sysmon_handler.beam", "test/rabbit_sysmon_minder.beam", "test/rabbit_table.beam", "test/rabbit_time_travel_dbg.beam", "test/rabbit_trace.beam", "test/rabbit_tracking.beam", "test/rabbit_tracking_store.beam", "test/rabbit_upgrade_preparation.beam", "test/rabbit_variable_queue.beam", "test/rabbit_version.beam", "test/rabbit_vhost.beam", "test/rabbit_vhost_limit.beam", "test/rabbit_vhost_msg_store.beam", "test/rabbit_vhost_process.beam", "test/rabbit_vhost_sup.beam", "test/rabbit_vhost_sup_sup.beam", "test/rabbit_vhost_sup_wrapper.beam", "test/rabbit_vm.beam", "test/supervised_lifecycle.beam", "test/tcp_listener.beam", "test/tcp_listener_sup.beam", "test/term_to_binary_compat.beam", "test/vhost.beam"],
    )
    erlang_bytecode(
        name = "test_code_server_cache_beam",
        testonly = True,
        srcs = ["src/code_server_cache.erl"],
        outs = ["test/code_server_cache.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_gatherer_beam",
        testonly = True,
        srcs = ["src/gatherer.erl"],
        outs = ["test/gatherer.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_gm_beam",
        testonly = True,
        srcs = ["src/gm.erl"],
        outs = ["test/gm.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_internal_user_beam",
        testonly = True,
        srcs = ["src/internal_user.erl"],
        outs = ["test/internal_user.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_lqueue_beam",
        testonly = True,
        srcs = ["src/lqueue.erl"],
        outs = ["test/lqueue.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_mirrored_supervisor_beam",
        testonly = True,
        srcs = ["src/mirrored_supervisor.erl"],
        outs = ["test/mirrored_supervisor.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_mirrored_supervisor_sups_beam",
        testonly = True,
        srcs = ["src/mirrored_supervisor_sups.erl"],
        outs = ["test/mirrored_supervisor_sups.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_pg_local_beam",
        testonly = True,
        srcs = ["src/pg_local.erl"],
        outs = ["test/pg_local.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_pid_recomposition_beam",
        testonly = True,
        srcs = ["src/pid_recomposition.erl"],
        outs = ["test/pid_recomposition.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_access_control_beam",
        testonly = True,
        srcs = ["src/rabbit_access_control.erl"],
        outs = ["test/rabbit_access_control.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_alarm_beam",
        testonly = True,
        srcs = ["src/rabbit_alarm.erl"],
        outs = ["test/rabbit_alarm.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_amqqueue_beam",
        testonly = True,
        srcs = ["src/rabbit_amqqueue.erl"],
        outs = ["test/rabbit_amqqueue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqqueue_process_beam",
        testonly = True,
        srcs = ["src/rabbit_amqqueue_process.erl"],
        outs = ["test/rabbit_amqqueue_process.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqqueue_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_amqqueue_sup.erl"],
        outs = ["test/rabbit_amqqueue_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_amqqueue_sup_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_amqqueue_sup_sup.erl"],
        outs = ["test/rabbit_amqqueue_sup_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_auth_backend_internal_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_backend_internal.erl"],
        outs = ["test/rabbit_auth_backend_internal.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_auth_mechanism_amqplain_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_mechanism_amqplain.erl"],
        outs = ["test/rabbit_auth_mechanism_amqplain.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_auth_mechanism_cr_demo_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_mechanism_cr_demo.erl"],
        outs = ["test/rabbit_auth_mechanism_cr_demo.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_auth_mechanism_plain_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_mechanism_plain.erl"],
        outs = ["test/rabbit_auth_mechanism_plain.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_autoheal_beam",
        testonly = True,
        srcs = ["src/rabbit_autoheal.erl"],
        outs = ["test/rabbit_autoheal.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_backing_queue_beam",
        testonly = True,
        srcs = ["src/rabbit_backing_queue.erl"],
        outs = ["test/rabbit_backing_queue.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_basic_beam",
        testonly = True,
        srcs = ["src/rabbit_basic.erl"],
        outs = ["test/rabbit_basic.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_beam",
        testonly = True,
        srcs = ["src/rabbit.erl"],
        outs = ["test/rabbit.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_binding_beam",
        testonly = True,
        srcs = ["src/rabbit_binding.erl"],
        outs = ["test/rabbit_binding.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_boot_steps_beam",
        testonly = True,
        srcs = ["src/rabbit_boot_steps.erl"],
        outs = ["test/rabbit_boot_steps.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_channel_beam",
        testonly = True,
        srcs = ["src/rabbit_channel.erl"],
        outs = ["test/rabbit_channel.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_channel_interceptor_beam",
        testonly = True,
        srcs = ["src/rabbit_channel_interceptor.erl"],
        outs = ["test/rabbit_channel_interceptor.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_channel_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_channel_sup.erl"],
        outs = ["test/rabbit_channel_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_channel_sup_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_channel_sup_sup.erl"],
        outs = ["test/rabbit_channel_sup_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_channel_tracking_beam",
        testonly = True,
        srcs = ["src/rabbit_channel_tracking.erl"],
        outs = ["test/rabbit_channel_tracking.beam"],
        beam = ["ebin/rabbit_tracking.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_channel_tracking_handler_beam",
        testonly = True,
        srcs = ["src/rabbit_channel_tracking_handler.erl"],
        outs = ["test/rabbit_channel_tracking_handler.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_classic_queue_beam",
        testonly = True,
        srcs = ["src/rabbit_classic_queue.erl"],
        outs = ["test/rabbit_classic_queue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_queue_type.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_classic_queue_index_v2_beam",
        testonly = True,
        srcs = ["src/rabbit_classic_queue_index_v2.erl"],
        outs = ["test/rabbit_classic_queue_index_v2.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_classic_queue_store_v2_beam",
        testonly = True,
        srcs = ["src/rabbit_classic_queue_store_v2.erl"],
        outs = ["test/rabbit_classic_queue_store_v2.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_client_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_client_sup.erl"],
        outs = ["test/rabbit_client_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_config_beam",
        testonly = True,
        srcs = ["src/rabbit_config.erl"],
        outs = ["test/rabbit_config.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_confirms_beam",
        testonly = True,
        srcs = ["src/rabbit_confirms.erl"],
        outs = ["test/rabbit_confirms.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_connection_helper_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_connection_helper_sup.erl"],
        outs = ["test/rabbit_connection_helper_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_connection_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_connection_sup.erl"],
        outs = ["test/rabbit_connection_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@ranch//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_connection_tracking_beam",
        testonly = True,
        srcs = ["src/rabbit_connection_tracking.erl"],
        outs = ["test/rabbit_connection_tracking.beam"],
        beam = ["ebin/rabbit_tracking.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_connection_tracking_handler_beam",
        testonly = True,
        srcs = ["src/rabbit_connection_tracking_handler.erl"],
        outs = ["test/rabbit_connection_tracking_handler.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_control_pbe_beam",
        testonly = True,
        srcs = ["src/rabbit_control_pbe.erl"],
        outs = ["test/rabbit_control_pbe.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_core_ff_beam",
        testonly = True,
        srcs = ["src/rabbit_core_ff.erl"],
        outs = ["test/rabbit_core_ff.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_core_metrics_gc_beam",
        testonly = True,
        srcs = ["src/rabbit_core_metrics_gc.erl"],
        outs = ["test/rabbit_core_metrics_gc.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_credential_validation_beam",
        testonly = True,
        srcs = ["src/rabbit_credential_validation.erl"],
        outs = ["test/rabbit_credential_validation.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_credential_validator_accept_everything_beam",
        testonly = True,
        srcs = ["src/rabbit_credential_validator_accept_everything.erl"],
        outs = ["test/rabbit_credential_validator_accept_everything.beam"],
        beam = ["ebin/rabbit_credential_validator.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_credential_validator_beam",
        testonly = True,
        srcs = ["src/rabbit_credential_validator.erl"],
        outs = ["test/rabbit_credential_validator.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_credential_validator_min_password_length_beam",
        testonly = True,
        srcs = ["src/rabbit_credential_validator_min_password_length.erl"],
        outs = ["test/rabbit_credential_validator_min_password_length.beam"],
        beam = ["ebin/rabbit_credential_validator.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_credential_validator_password_regexp_beam",
        testonly = True,
        srcs = ["src/rabbit_credential_validator_password_regexp.erl"],
        outs = ["test/rabbit_credential_validator_password_regexp.beam"],
        beam = ["ebin/rabbit_credential_validator.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_dead_letter_beam",
        testonly = True,
        srcs = ["src/rabbit_dead_letter.erl"],
        outs = ["test/rabbit_dead_letter.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_definitions_beam",
        testonly = True,
        srcs = ["src/rabbit_definitions.erl"],
        outs = ["test/rabbit_definitions.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_definitions_hashing_beam",
        testonly = True,
        srcs = ["src/rabbit_definitions_hashing.erl"],
        outs = ["test/rabbit_definitions_hashing.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_definitions_import_https_beam",
        testonly = True,
        srcs = ["src/rabbit_definitions_import_https.erl"],
        outs = ["test/rabbit_definitions_import_https.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_definitions_import_local_filesystem_beam",
        testonly = True,
        srcs = ["src/rabbit_definitions_import_local_filesystem.erl"],
        outs = ["test/rabbit_definitions_import_local_filesystem.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_diagnostics_beam",
        testonly = True,
        srcs = ["src/rabbit_diagnostics.erl"],
        outs = ["test/rabbit_diagnostics.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_direct_beam",
        testonly = True,
        srcs = ["src/rabbit_direct.erl"],
        outs = ["test/rabbit_direct.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_direct_reply_to_beam",
        testonly = True,
        srcs = ["src/rabbit_direct_reply_to.erl"],
        outs = ["test/rabbit_direct_reply_to.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_disk_monitor_beam",
        testonly = True,
        srcs = ["src/rabbit_disk_monitor.erl"],
        outs = ["test/rabbit_disk_monitor.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_epmd_monitor_beam",
        testonly = True,
        srcs = ["src/rabbit_epmd_monitor.erl"],
        outs = ["test/rabbit_epmd_monitor.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_event_consumer_beam",
        testonly = True,
        srcs = ["src/rabbit_event_consumer.erl"],
        outs = ["test/rabbit_event_consumer.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_exchange_beam",
        testonly = True,
        srcs = ["src/rabbit_exchange.erl"],
        outs = ["test/rabbit_exchange.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_exchange_decorator_beam",
        testonly = True,
        srcs = ["src/rabbit_exchange_decorator.erl"],
        outs = ["test/rabbit_exchange_decorator.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_exchange_parameters_beam",
        testonly = True,
        srcs = ["src/rabbit_exchange_parameters.erl"],
        outs = ["test/rabbit_exchange_parameters.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_exchange_type_direct_beam",
        testonly = True,
        srcs = ["src/rabbit_exchange_type_direct.erl"],
        outs = ["test/rabbit_exchange_type_direct.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_exchange_type_fanout_beam",
        testonly = True,
        srcs = ["src/rabbit_exchange_type_fanout.erl"],
        outs = ["test/rabbit_exchange_type_fanout.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_exchange_type_headers_beam",
        testonly = True,
        srcs = ["src/rabbit_exchange_type_headers.erl"],
        outs = ["test/rabbit_exchange_type_headers.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_exchange_type_invalid_beam",
        testonly = True,
        srcs = ["src/rabbit_exchange_type_invalid.erl"],
        outs = ["test/rabbit_exchange_type_invalid.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_exchange_type_topic_beam",
        testonly = True,
        srcs = ["src/rabbit_exchange_type_topic.erl"],
        outs = ["test/rabbit_exchange_type_topic.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_feature_flags_beam",
        testonly = True,
        srcs = ["src/rabbit_feature_flags.erl"],
        outs = ["test/rabbit_feature_flags.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_ff_controller_beam",
        testonly = True,
        srcs = ["src/rabbit_ff_controller.erl"],
        outs = ["test/rabbit_ff_controller.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_ff_extra_beam",
        testonly = True,
        srcs = ["src/rabbit_ff_extra.erl"],
        outs = ["test/rabbit_ff_extra.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@stdout_formatter//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_ff_registry_beam",
        testonly = True,
        srcs = ["src/rabbit_ff_registry.erl"],
        outs = ["test/rabbit_ff_registry.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_ff_registry_factory_beam",
        testonly = True,
        srcs = ["src/rabbit_ff_registry_factory.erl"],
        outs = ["test/rabbit_ff_registry_factory.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_fhc_helpers_beam",
        testonly = True,
        srcs = ["src/rabbit_fhc_helpers.erl"],
        outs = ["test/rabbit_fhc_helpers.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_fifo_beam",
        testonly = True,
        srcs = ["src/rabbit_fifo.erl"],
        outs = ["test/rabbit_fifo.beam"],
        hdrs = ["src/rabbit_fifo.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@ra//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_fifo_client_beam",
        testonly = True,
        srcs = ["src/rabbit_fifo_client.erl"],
        outs = ["test/rabbit_fifo_client.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_fifo_dlx_beam",
        testonly = True,
        srcs = ["src/rabbit_fifo_dlx.erl"],
        outs = ["test/rabbit_fifo_dlx.beam"],
        hdrs = ["src/rabbit_fifo.hrl", "src/rabbit_fifo_dlx.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_fifo_dlx_client_beam",
        testonly = True,
        srcs = ["src/rabbit_fifo_dlx_client.erl"],
        outs = ["test/rabbit_fifo_dlx_client.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_fifo_dlx_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_fifo_dlx_sup.erl"],
        outs = ["test/rabbit_fifo_dlx_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_fifo_dlx_worker_beam",
        testonly = True,
        srcs = ["src/rabbit_fifo_dlx_worker.erl"],
        outs = ["test/rabbit_fifo_dlx_worker.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_fifo_index_beam",
        testonly = True,
        srcs = ["src/rabbit_fifo_index.erl"],
        outs = ["test/rabbit_fifo_index.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_fifo_v0_beam",
        testonly = True,
        srcs = ["src/rabbit_fifo_v0.erl"],
        outs = ["test/rabbit_fifo_v0.beam"],
        hdrs = ["src/rabbit_fifo_v0.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@ra//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_fifo_v1_beam",
        testonly = True,
        srcs = ["src/rabbit_fifo_v1.erl"],
        outs = ["test/rabbit_fifo_v1.beam"],
        hdrs = ["src/rabbit_fifo_v1.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@ra//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_file_beam",
        testonly = True,
        srcs = ["src/rabbit_file.erl"],
        outs = ["test/rabbit_file.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_framing_beam",
        testonly = True,
        srcs = ["src/rabbit_framing.erl"],
        outs = ["test/rabbit_framing.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_global_counters_beam",
        testonly = True,
        srcs = ["src/rabbit_global_counters.erl"],
        outs = ["test/rabbit_global_counters.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_guid_beam",
        testonly = True,
        srcs = ["src/rabbit_guid.erl"],
        outs = ["test/rabbit_guid.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_health_check_beam",
        testonly = True,
        srcs = ["src/rabbit_health_check.erl"],
        outs = ["test/rabbit_health_check.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_limiter_beam",
        testonly = True,
        srcs = ["src/rabbit_limiter.erl"],
        outs = ["test/rabbit_limiter.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_log_channel_beam",
        testonly = True,
        srcs = ["src/rabbit_log_channel.erl"],
        outs = ["test/rabbit_log_channel.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_log_connection_beam",
        testonly = True,
        srcs = ["src/rabbit_log_connection.erl"],
        outs = ["test/rabbit_log_connection.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_log_feature_flags_beam",
        testonly = True,
        srcs = ["src/rabbit_log_feature_flags.erl"],
        outs = ["test/rabbit_log_feature_flags.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_log_mirroring_beam",
        testonly = True,
        srcs = ["src/rabbit_log_mirroring.erl"],
        outs = ["test/rabbit_log_mirroring.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_log_prelaunch_beam",
        testonly = True,
        srcs = ["src/rabbit_log_prelaunch.erl"],
        outs = ["test/rabbit_log_prelaunch.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_log_queue_beam",
        testonly = True,
        srcs = ["src/rabbit_log_queue.erl"],
        outs = ["test/rabbit_log_queue.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_log_tail_beam",
        testonly = True,
        srcs = ["src/rabbit_log_tail.erl"],
        outs = ["test/rabbit_log_tail.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_logger_exchange_h_beam",
        testonly = True,
        srcs = ["src/rabbit_logger_exchange_h.erl"],
        outs = ["test/rabbit_logger_exchange_h.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_looking_glass_beam",
        testonly = True,
        srcs = ["src/rabbit_looking_glass.erl"],
        outs = ["test/rabbit_looking_glass.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_maintenance_beam",
        testonly = True,
        srcs = ["src/rabbit_maintenance.erl"],
        outs = ["test/rabbit_maintenance.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_memory_monitor_beam",
        testonly = True,
        srcs = ["src/rabbit_memory_monitor.erl"],
        outs = ["test/rabbit_memory_monitor.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_metrics_beam",
        testonly = True,
        srcs = ["src/rabbit_metrics.erl"],
        outs = ["test/rabbit_metrics.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mirror_queue_coordinator_beam",
        testonly = True,
        srcs = ["src/rabbit_mirror_queue_coordinator.erl"],
        outs = ["test/rabbit_mirror_queue_coordinator.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl", "include/gm_specs.hrl"],
        beam = ["ebin/gm.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mirror_queue_master_beam",
        testonly = True,
        srcs = ["src/rabbit_mirror_queue_master.erl"],
        outs = ["test/rabbit_mirror_queue_master.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_backing_queue.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mirror_queue_misc_beam",
        testonly = True,
        srcs = ["src/rabbit_mirror_queue_misc.erl"],
        outs = ["test/rabbit_mirror_queue_misc.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mirror_queue_mode_all_beam",
        testonly = True,
        srcs = ["src/rabbit_mirror_queue_mode_all.erl"],
        outs = ["test/rabbit_mirror_queue_mode_all.beam"],
        beam = ["ebin/rabbit_mirror_queue_mode.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mirror_queue_mode_beam",
        testonly = True,
        srcs = ["src/rabbit_mirror_queue_mode.erl"],
        outs = ["test/rabbit_mirror_queue_mode.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mirror_queue_mode_exactly_beam",
        testonly = True,
        srcs = ["src/rabbit_mirror_queue_mode_exactly.erl"],
        outs = ["test/rabbit_mirror_queue_mode_exactly.beam"],
        beam = ["ebin/rabbit_mirror_queue_mode.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mirror_queue_mode_nodes_beam",
        testonly = True,
        srcs = ["src/rabbit_mirror_queue_mode_nodes.erl"],
        outs = ["test/rabbit_mirror_queue_mode_nodes.beam"],
        beam = ["ebin/rabbit_mirror_queue_mode.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mirror_queue_slave_beam",
        testonly = True,
        srcs = ["src/rabbit_mirror_queue_slave.erl"],
        outs = ["test/rabbit_mirror_queue_slave.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl", "include/gm_specs.hrl"],
        beam = ["ebin/gm.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mirror_queue_sync_beam",
        testonly = True,
        srcs = ["src/rabbit_mirror_queue_sync.erl"],
        outs = ["test/rabbit_mirror_queue_sync.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_mnesia_beam",
        testonly = True,
        srcs = ["src/rabbit_mnesia.erl"],
        outs = ["test/rabbit_mnesia.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_mnesia_rename_beam",
        testonly = True,
        srcs = ["src/rabbit_mnesia_rename.erl"],
        outs = ["test/rabbit_mnesia_rename.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_msg_file_beam",
        testonly = True,
        srcs = ["src/rabbit_msg_file.erl"],
        outs = ["test/rabbit_msg_file.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_msg_record_beam",
        testonly = True,
        srcs = ["src/rabbit_msg_record.erl"],
        outs = ["test/rabbit_msg_record.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp10_common:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_msg_store_beam",
        testonly = True,
        srcs = ["src/rabbit_msg_store.erl"],
        outs = ["test/rabbit_msg_store.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_msg_store_ets_index_beam",
        testonly = True,
        srcs = ["src/rabbit_msg_store_ets_index.erl"],
        outs = ["test/rabbit_msg_store_ets_index.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_msg_store_gc_beam",
        testonly = True,
        srcs = ["src/rabbit_msg_store_gc.erl"],
        outs = ["test/rabbit_msg_store_gc.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_networking_beam",
        testonly = True,
        srcs = ["src/rabbit_networking.erl"],
        outs = ["test/rabbit_networking.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_networking_store_beam",
        testonly = True,
        srcs = ["src/rabbit_networking_store.erl"],
        outs = ["test/rabbit_networking_store.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_node_monitor_beam",
        testonly = True,
        srcs = ["src/rabbit_node_monitor.erl"],
        outs = ["test/rabbit_node_monitor.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_nodes_beam",
        testonly = True,
        srcs = ["src/rabbit_nodes.erl"],
        outs = ["test/rabbit_nodes.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_observer_cli_beam",
        testonly = True,
        srcs = ["src/rabbit_observer_cli.erl"],
        outs = ["test/rabbit_observer_cli.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_observer_cli_classic_queues_beam",
        testonly = True,
        srcs = ["src/rabbit_observer_cli_classic_queues.erl"],
        outs = ["test/rabbit_observer_cli_classic_queues.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_osiris_metrics_beam",
        testonly = True,
        srcs = ["src/rabbit_osiris_metrics.erl"],
        outs = ["test/rabbit_osiris_metrics.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_parameter_validation_beam",
        testonly = True,
        srcs = ["src/rabbit_parameter_validation.erl"],
        outs = ["test/rabbit_parameter_validation.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_password_beam",
        testonly = True,
        srcs = ["src/rabbit_password.erl"],
        outs = ["test/rabbit_password.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_password_hashing_md5_beam",
        testonly = True,
        srcs = ["src/rabbit_password_hashing_md5.erl"],
        outs = ["test/rabbit_password_hashing_md5.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_password_hashing_sha256_beam",
        testonly = True,
        srcs = ["src/rabbit_password_hashing_sha256.erl"],
        outs = ["test/rabbit_password_hashing_sha256.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_password_hashing_sha512_beam",
        testonly = True,
        srcs = ["src/rabbit_password_hashing_sha512.erl"],
        outs = ["test/rabbit_password_hashing_sha512.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_peer_discovery_beam",
        testonly = True,
        srcs = ["src/rabbit_peer_discovery.erl"],
        outs = ["test/rabbit_peer_discovery.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_peer_discovery_classic_config_beam",
        testonly = True,
        srcs = ["src/rabbit_peer_discovery_classic_config.erl"],
        outs = ["test/rabbit_peer_discovery_classic_config.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_peer_discovery_dns_beam",
        testonly = True,
        srcs = ["src/rabbit_peer_discovery_dns.erl"],
        outs = ["test/rabbit_peer_discovery_dns.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_plugins_beam",
        testonly = True,
        srcs = ["src/rabbit_plugins.erl"],
        outs = ["test/rabbit_plugins.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_policies_beam",
        testonly = True,
        srcs = ["src/rabbit_policies.erl"],
        outs = ["test/rabbit_policies.beam"],
        beam = ["ebin/rabbit_policy_merge_strategy.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_policy_beam",
        testonly = True,
        srcs = ["src/rabbit_policy.erl"],
        outs = ["test/rabbit_policy.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_policy_merge_strategy_beam",
        testonly = True,
        srcs = ["src/rabbit_policy_merge_strategy.erl"],
        outs = ["test/rabbit_policy_merge_strategy.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_prelaunch_cluster_beam",
        testonly = True,
        srcs = ["src/rabbit_prelaunch_cluster.erl"],
        outs = ["test/rabbit_prelaunch_cluster.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_prelaunch_enabled_plugins_file_beam",
        testonly = True,
        srcs = ["src/rabbit_prelaunch_enabled_plugins_file.erl"],
        outs = ["test/rabbit_prelaunch_enabled_plugins_file.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_prelaunch_feature_flags_beam",
        testonly = True,
        srcs = ["src/rabbit_prelaunch_feature_flags.erl"],
        outs = ["test/rabbit_prelaunch_feature_flags.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_prelaunch_logging_beam",
        testonly = True,
        srcs = ["src/rabbit_prelaunch_logging.erl"],
        outs = ["test/rabbit_prelaunch_logging.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_prequeue_beam",
        testonly = True,
        srcs = ["src/rabbit_prequeue.erl"],
        outs = ["test/rabbit_prequeue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_priority_queue_beam",
        testonly = True,
        srcs = ["src/rabbit_priority_queue.erl"],
        outs = ["test/rabbit_priority_queue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_backing_queue.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_queue_consumers_beam",
        testonly = True,
        srcs = ["src/rabbit_queue_consumers.erl"],
        outs = ["test/rabbit_queue_consumers.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_queue_decorator_beam",
        testonly = True,
        srcs = ["src/rabbit_queue_decorator.erl"],
        outs = ["test/rabbit_queue_decorator.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_queue_index_beam",
        testonly = True,
        srcs = ["src/rabbit_queue_index.erl"],
        outs = ["test/rabbit_queue_index.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_queue_location_beam",
        testonly = True,
        srcs = ["src/rabbit_queue_location.erl"],
        outs = ["test/rabbit_queue_location.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_queue_location_client_local_beam",
        testonly = True,
        srcs = ["src/rabbit_queue_location_client_local.erl"],
        outs = ["test/rabbit_queue_location_client_local.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_queue_master_locator.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_queue_location_min_masters_beam",
        testonly = True,
        srcs = ["src/rabbit_queue_location_min_masters.erl"],
        outs = ["test/rabbit_queue_location_min_masters.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_queue_master_locator.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_queue_location_random_beam",
        testonly = True,
        srcs = ["src/rabbit_queue_location_random.erl"],
        outs = ["test/rabbit_queue_location_random.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_queue_master_locator.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_queue_location_validator_beam",
        testonly = True,
        srcs = ["src/rabbit_queue_location_validator.erl"],
        outs = ["test/rabbit_queue_location_validator.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_queue_master_location_misc_beam",
        testonly = True,
        srcs = ["src/rabbit_queue_master_location_misc.erl"],
        outs = ["test/rabbit_queue_master_location_misc.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_queue_master_locator_beam",
        testonly = True,
        srcs = ["src/rabbit_queue_master_locator.erl"],
        outs = ["test/rabbit_queue_master_locator.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_queue_type_beam",
        testonly = True,
        srcs = ["src/rabbit_queue_type.erl"],
        outs = ["test/rabbit_queue_type.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_queue_type_util_beam",
        testonly = True,
        srcs = ["src/rabbit_queue_type_util.erl"],
        outs = ["test/rabbit_queue_type_util.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_quorum_memory_manager_beam",
        testonly = True,
        srcs = ["src/rabbit_quorum_memory_manager.erl"],
        outs = ["test/rabbit_quorum_memory_manager.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_quorum_queue_beam",
        testonly = True,
        srcs = ["src/rabbit_quorum_queue.erl"],
        outs = ["test/rabbit_quorum_queue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_queue_type.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_ra_registry_beam",
        testonly = True,
        srcs = ["src/rabbit_ra_registry.erl"],
        outs = ["test/rabbit_ra_registry.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_ra_systems_beam",
        testonly = True,
        srcs = ["src/rabbit_ra_systems.erl"],
        outs = ["test/rabbit_ra_systems.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_reader_beam",
        testonly = True,
        srcs = ["src/rabbit_reader.erl"],
        outs = ["test/rabbit_reader.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_recovery_terms_beam",
        testonly = True,
        srcs = ["src/rabbit_recovery_terms.erl"],
        outs = ["test/rabbit_recovery_terms.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_release_series_beam",
        testonly = True,
        srcs = ["src/rabbit_release_series.erl"],
        outs = ["test/rabbit_release_series.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_restartable_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_restartable_sup.erl"],
        outs = ["test/rabbit_restartable_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_router_beam",
        testonly = True,
        srcs = ["src/rabbit_router.erl"],
        outs = ["test/rabbit_router.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_runtime_parameters_beam",
        testonly = True,
        srcs = ["src/rabbit_runtime_parameters.erl"],
        outs = ["test/rabbit_runtime_parameters.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_ssl_beam",
        testonly = True,
        srcs = ["src/rabbit_ssl.erl"],
        outs = ["test/rabbit_ssl.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_stream_coordinator_beam",
        testonly = True,
        srcs = ["src/rabbit_stream_coordinator.erl"],
        outs = ["test/rabbit_stream_coordinator.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl", "src/rabbit_stream_coordinator.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@ra//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_stream_queue_beam",
        testonly = True,
        srcs = ["src/rabbit_stream_queue.erl"],
        outs = ["test/rabbit_stream_queue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_queue_type.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_stream_sac_coordinator_beam",
        testonly = True,
        srcs = ["src/rabbit_stream_sac_coordinator.erl"],
        outs = ["test/rabbit_stream_sac_coordinator.beam"],
        hdrs = ["src/rabbit_stream_sac_coordinator.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_sup.erl"],
        outs = ["test/rabbit_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_sysmon_handler_beam",
        testonly = True,
        srcs = ["src/rabbit_sysmon_handler.erl"],
        outs = ["test/rabbit_sysmon_handler.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_sysmon_minder_beam",
        testonly = True,
        srcs = ["src/rabbit_sysmon_minder.erl"],
        outs = ["test/rabbit_sysmon_minder.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_table_beam",
        testonly = True,
        srcs = ["src/rabbit_table.erl"],
        outs = ["test/rabbit_table.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_time_travel_dbg_beam",
        testonly = True,
        srcs = ["src/rabbit_time_travel_dbg.erl"],
        outs = ["test/rabbit_time_travel_dbg.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_trace_beam",
        testonly = True,
        srcs = ["src/rabbit_trace.erl"],
        outs = ["test/rabbit_trace.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_tracking_beam",
        testonly = True,
        srcs = ["src/rabbit_tracking.erl"],
        outs = ["test/rabbit_tracking.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_tracking_store_beam",
        testonly = True,
        srcs = ["src/rabbit_tracking_store.erl"],
        outs = ["test/rabbit_tracking_store.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_upgrade_preparation_beam",
        testonly = True,
        srcs = ["src/rabbit_upgrade_preparation.erl"],
        outs = ["test/rabbit_upgrade_preparation.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_variable_queue_beam",
        testonly = True,
        srcs = ["src/rabbit_variable_queue.erl"],
        outs = ["test/rabbit_variable_queue.beam"],
        hdrs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl"],
        beam = ["ebin/rabbit_backing_queue.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_version_beam",
        testonly = True,
        srcs = ["src/rabbit_version.erl"],
        outs = ["test/rabbit_version.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_vhost_beam",
        testonly = True,
        srcs = ["src/rabbit_vhost.erl"],
        outs = ["test/rabbit_vhost.beam"],
        hdrs = ["include/vhost.hrl", "include/vhost_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_vhost_limit_beam",
        testonly = True,
        srcs = ["src/rabbit_vhost_limit.erl"],
        outs = ["test/rabbit_vhost_limit.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_vhost_msg_store_beam",
        testonly = True,
        srcs = ["src/rabbit_vhost_msg_store.erl"],
        outs = ["test/rabbit_vhost_msg_store.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_vhost_process_beam",
        testonly = True,
        srcs = ["src/rabbit_vhost_process.erl"],
        outs = ["test/rabbit_vhost_process.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_vhost_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_vhost_sup.erl"],
        outs = ["test/rabbit_vhost_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_vhost_sup_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_vhost_sup_sup.erl"],
        outs = ["test/rabbit_vhost_sup_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_vhost_sup_wrapper_beam",
        testonly = True,
        srcs = ["src/rabbit_vhost_sup_wrapper.erl"],
        outs = ["test/rabbit_vhost_sup_wrapper.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_vm_beam",
        testonly = True,
        srcs = ["src/rabbit_vm.erl"],
        outs = ["test/rabbit_vm.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_supervised_lifecycle_beam",
        testonly = True,
        srcs = ["src/supervised_lifecycle.erl"],
        outs = ["test/supervised_lifecycle.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_tcp_listener_beam",
        testonly = True,
        srcs = ["src/tcp_listener.erl"],
        outs = ["test/tcp_listener.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_tcp_listener_sup_beam",
        testonly = True,
        srcs = ["src/tcp_listener_sup.erl"],
        outs = ["test/tcp_listener_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_term_to_binary_compat_beam",
        testonly = True,
        srcs = ["src/term_to_binary_compat.erl"],
        outs = ["test/term_to_binary_compat.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_vhost_beam",
        testonly = True,
        srcs = ["src/vhost.erl"],
        outs = ["test/vhost.beam"],
        hdrs = ["include/vhost.hrl", "include/vhost_v2.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/amqqueue.hrl", "include/amqqueue_v2.hrl", "include/gm_specs.hrl", "include/rabbit_global_counters.hrl", "include/vhost.hrl", "include/vhost_v2.hrl", "src/amqqueue.erl", "src/background_gc.erl", "src/code_server_cache.erl", "src/gatherer.erl", "src/gm.erl", "src/internal_user.erl", "src/lqueue.erl", "src/mirrored_supervisor.erl", "src/mirrored_supervisor_sups.erl", "src/pg_local.erl", "src/pid_recomposition.erl", "src/rabbit.erl", "src/rabbit_access_control.erl", "src/rabbit_alarm.erl", "src/rabbit_amqqueue.erl", "src/rabbit_amqqueue_process.erl", "src/rabbit_amqqueue_sup.erl", "src/rabbit_amqqueue_sup_sup.erl", "src/rabbit_auth_backend_internal.erl", "src/rabbit_auth_mechanism_amqplain.erl", "src/rabbit_auth_mechanism_cr_demo.erl", "src/rabbit_auth_mechanism_plain.erl", "src/rabbit_autoheal.erl", "src/rabbit_backing_queue.erl", "src/rabbit_basic.erl", "src/rabbit_binding.erl", "src/rabbit_boot_steps.erl", "src/rabbit_channel.erl", "src/rabbit_channel_interceptor.erl", "src/rabbit_channel_sup.erl", "src/rabbit_channel_sup_sup.erl", "src/rabbit_channel_tracking.erl", "src/rabbit_channel_tracking_handler.erl", "src/rabbit_classic_queue.erl", "src/rabbit_classic_queue_index_v2.erl", "src/rabbit_classic_queue_store_v2.erl", "src/rabbit_client_sup.erl", "src/rabbit_config.erl", "src/rabbit_confirms.erl", "src/rabbit_connection_helper_sup.erl", "src/rabbit_connection_sup.erl", "src/rabbit_connection_tracking.erl", "src/rabbit_connection_tracking_handler.erl", "src/rabbit_control_pbe.erl", "src/rabbit_core_ff.erl", "src/rabbit_core_metrics_gc.erl", "src/rabbit_credential_validation.erl", "src/rabbit_credential_validator.erl", "src/rabbit_credential_validator_accept_everything.erl", "src/rabbit_credential_validator_min_password_length.erl", "src/rabbit_credential_validator_password_regexp.erl", "src/rabbit_dead_letter.erl", "src/rabbit_definitions.erl", "src/rabbit_definitions_hashing.erl", "src/rabbit_definitions_import_https.erl", "src/rabbit_definitions_import_local_filesystem.erl", "src/rabbit_diagnostics.erl", "src/rabbit_direct.erl", "src/rabbit_direct_reply_to.erl", "src/rabbit_disk_monitor.erl", "src/rabbit_epmd_monitor.erl", "src/rabbit_event_consumer.erl", "src/rabbit_exchange.erl", "src/rabbit_exchange_decorator.erl", "src/rabbit_exchange_parameters.erl", "src/rabbit_exchange_type_direct.erl", "src/rabbit_exchange_type_fanout.erl", "src/rabbit_exchange_type_headers.erl", "src/rabbit_exchange_type_invalid.erl", "src/rabbit_exchange_type_topic.erl", "src/rabbit_feature_flags.erl", "src/rabbit_ff_controller.erl", "src/rabbit_ff_extra.erl", "src/rabbit_ff_registry.erl", "src/rabbit_ff_registry_factory.erl", "src/rabbit_fhc_helpers.erl", "src/rabbit_fifo.erl", "src/rabbit_fifo.hrl", "src/rabbit_fifo_client.erl", "src/rabbit_fifo_dlx.erl", "src/rabbit_fifo_dlx.hrl", "src/rabbit_fifo_dlx_client.erl", "src/rabbit_fifo_dlx_sup.erl", "src/rabbit_fifo_dlx_worker.erl", "src/rabbit_fifo_index.erl", "src/rabbit_fifo_v0.erl", "src/rabbit_fifo_v0.hrl", "src/rabbit_fifo_v1.erl", "src/rabbit_fifo_v1.hrl", "src/rabbit_file.erl", "src/rabbit_framing.erl", "src/rabbit_global_counters.erl", "src/rabbit_guid.erl", "src/rabbit_health_check.erl", "src/rabbit_limiter.erl", "src/rabbit_log_channel.erl", "src/rabbit_log_connection.erl", "src/rabbit_log_feature_flags.erl", "src/rabbit_log_mirroring.erl", "src/rabbit_log_prelaunch.erl", "src/rabbit_log_queue.erl", "src/rabbit_log_tail.erl", "src/rabbit_logger_exchange_h.erl", "src/rabbit_looking_glass.erl", "src/rabbit_maintenance.erl", "src/rabbit_memory_monitor.erl", "src/rabbit_metrics.erl", "src/rabbit_mirror_queue_coordinator.erl", "src/rabbit_mirror_queue_master.erl", "src/rabbit_mirror_queue_misc.erl", "src/rabbit_mirror_queue_mode.erl", "src/rabbit_mirror_queue_mode_all.erl", "src/rabbit_mirror_queue_mode_exactly.erl", "src/rabbit_mirror_queue_mode_nodes.erl", "src/rabbit_mirror_queue_slave.erl", "src/rabbit_mirror_queue_sync.erl", "src/rabbit_mnesia.erl", "src/rabbit_mnesia_rename.erl", "src/rabbit_msg_file.erl", "src/rabbit_msg_record.erl", "src/rabbit_msg_store.erl", "src/rabbit_msg_store_ets_index.erl", "src/rabbit_msg_store_gc.erl", "src/rabbit_networking.erl", "src/rabbit_networking_store.erl", "src/rabbit_node_monitor.erl", "src/rabbit_nodes.erl", "src/rabbit_observer_cli.erl", "src/rabbit_observer_cli_classic_queues.erl", "src/rabbit_osiris_metrics.erl", "src/rabbit_parameter_validation.erl", "src/rabbit_password.erl", "src/rabbit_password_hashing_md5.erl", "src/rabbit_password_hashing_sha256.erl", "src/rabbit_password_hashing_sha512.erl", "src/rabbit_peer_discovery.erl", "src/rabbit_peer_discovery_classic_config.erl", "src/rabbit_peer_discovery_dns.erl", "src/rabbit_plugins.erl", "src/rabbit_policies.erl", "src/rabbit_policy.erl", "src/rabbit_policy_merge_strategy.erl", "src/rabbit_prelaunch_cluster.erl", "src/rabbit_prelaunch_enabled_plugins_file.erl", "src/rabbit_prelaunch_feature_flags.erl", "src/rabbit_prelaunch_logging.erl", "src/rabbit_prequeue.erl", "src/rabbit_priority_queue.erl", "src/rabbit_queue_consumers.erl", "src/rabbit_queue_decorator.erl", "src/rabbit_queue_index.erl", "src/rabbit_queue_location.erl", "src/rabbit_queue_location_client_local.erl", "src/rabbit_queue_location_min_masters.erl", "src/rabbit_queue_location_random.erl", "src/rabbit_queue_location_validator.erl", "src/rabbit_queue_master_location_misc.erl", "src/rabbit_queue_master_locator.erl", "src/rabbit_queue_type.erl", "src/rabbit_queue_type_util.erl", "src/rabbit_quorum_memory_manager.erl", "src/rabbit_quorum_queue.erl", "src/rabbit_ra_registry.erl", "src/rabbit_ra_systems.erl", "src/rabbit_reader.erl", "src/rabbit_recovery_terms.erl", "src/rabbit_release_series.erl", "src/rabbit_restartable_sup.erl", "src/rabbit_router.erl", "src/rabbit_runtime_parameters.erl", "src/rabbit_ssl.erl", "src/rabbit_stream_coordinator.erl", "src/rabbit_stream_coordinator.hrl", "src/rabbit_stream_queue.erl", "src/rabbit_stream_sac_coordinator.erl", "src/rabbit_stream_sac_coordinator.hrl", "src/rabbit_sup.erl", "src/rabbit_sysmon_handler.erl", "src/rabbit_sysmon_minder.erl", "src/rabbit_table.erl", "src/rabbit_time_travel_dbg.erl", "src/rabbit_trace.erl", "src/rabbit_tracking.erl", "src/rabbit_tracking_store.erl", "src/rabbit_upgrade_preparation.erl", "src/rabbit_variable_queue.erl", "src/rabbit_version.erl", "src/rabbit_vhost.erl", "src/rabbit_vhost_limit.erl", "src/rabbit_vhost_msg_store.erl", "src/rabbit_vhost_process.erl", "src/rabbit_vhost_sup.erl", "src/rabbit_vhost_sup_sup.erl", "src/rabbit_vhost_sup_wrapper.erl", "src/rabbit_vm.erl", "src/supervised_lifecycle.erl", "src/tcp_listener.erl", "src/tcp_listener_sup.erl", "src/term_to_binary_compat.erl", "src/vhost.erl"],
    )
