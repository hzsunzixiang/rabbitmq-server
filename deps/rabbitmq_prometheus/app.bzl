load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/collectors/prometheus_process_collector.beam", "ebin/collectors/prometheus_rabbitmq_alarm_metrics_collector.beam", "ebin/collectors/prometheus_rabbitmq_core_metrics_collector.beam", "ebin/collectors/prometheus_rabbitmq_global_metrics_collector.beam", "ebin/rabbit_prometheus_app.beam", "ebin/rabbit_prometheus_dispatcher.beam", "ebin/rabbit_prometheus_handler.beam"],
    )
    erlang_bytecode(
        name = "ebin_collectors_prometheus_process_collector_beam",
        srcs = ["src/collectors/prometheus_process_collector.erl"],
        outs = ["ebin/collectors/prometheus_process_collector.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["@prometheus//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_collectors_prometheus_rabbitmq_alarm_metrics_collector_beam",
        srcs = ["src/collectors/prometheus_rabbitmq_alarm_metrics_collector.erl"],
        outs = ["ebin/collectors/prometheus_rabbitmq_alarm_metrics_collector.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["@prometheus//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_collectors_prometheus_rabbitmq_core_metrics_collector_beam",
        srcs = ["src/collectors/prometheus_rabbitmq_core_metrics_collector.erl"],
        outs = ["ebin/collectors/prometheus_rabbitmq_core_metrics_collector.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@prometheus//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_collectors_prometheus_rabbitmq_global_metrics_collector_beam",
        srcs = ["src/collectors/prometheus_rabbitmq_global_metrics_collector.erl"],
        outs = ["ebin/collectors/prometheus_rabbitmq_global_metrics_collector.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["@prometheus//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_prometheus_app_beam",
        srcs = ["src/rabbit_prometheus_app.erl"],
        outs = ["ebin/rabbit_prometheus_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_prometheus_dispatcher_beam",
        srcs = ["src/rabbit_prometheus_dispatcher.erl"],
        outs = ["ebin/rabbit_prometheus_dispatcher.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_prometheus_handler_beam",
        srcs = ["src/rabbit_prometheus_handler.erl"],
        outs = ["ebin/rabbit_prometheus_handler.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/collectors/prometheus_process_collector.beam", "test/collectors/prometheus_rabbitmq_alarm_metrics_collector.beam", "test/collectors/prometheus_rabbitmq_core_metrics_collector.beam", "test/collectors/prometheus_rabbitmq_global_metrics_collector.beam", "test/rabbit_prometheus_app.beam", "test/rabbit_prometheus_dispatcher.beam", "test/rabbit_prometheus_handler.beam"],
    )
    erlang_bytecode(
        name = "test_collectors_prometheus_process_collector_beam",
        testonly = True,
        srcs = ["src/collectors/prometheus_process_collector.erl"],
        outs = ["test/collectors/prometheus_process_collector.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@prometheus//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_collectors_prometheus_rabbitmq_alarm_metrics_collector_beam",
        testonly = True,
        srcs = ["src/collectors/prometheus_rabbitmq_alarm_metrics_collector.erl"],
        outs = ["test/collectors/prometheus_rabbitmq_alarm_metrics_collector.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@prometheus//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_collectors_prometheus_rabbitmq_core_metrics_collector_beam",
        testonly = True,
        srcs = ["src/collectors/prometheus_rabbitmq_core_metrics_collector.erl"],
        outs = ["test/collectors/prometheus_rabbitmq_core_metrics_collector.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app", "@prometheus//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_collectors_prometheus_rabbitmq_global_metrics_collector_beam",
        testonly = True,
        srcs = ["src/collectors/prometheus_rabbitmq_global_metrics_collector.erl"],
        outs = ["test/collectors/prometheus_rabbitmq_global_metrics_collector.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@prometheus//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_prometheus_app_beam",
        testonly = True,
        srcs = ["src/rabbit_prometheus_app.erl"],
        outs = ["test/rabbit_prometheus_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_prometheus_dispatcher_beam",
        testonly = True,
        srcs = ["src/rabbit_prometheus_dispatcher.erl"],
        outs = ["test/rabbit_prometheus_dispatcher.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_prometheus_handler_beam",
        testonly = True,
        srcs = ["src/rabbit_prometheus_handler.erl"],
        outs = ["test/rabbit_prometheus_handler.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/collectors/prometheus_process_collector.erl", "src/collectors/prometheus_rabbitmq_alarm_metrics_collector.erl", "src/collectors/prometheus_rabbitmq_core_metrics_collector.erl", "src/collectors/prometheus_rabbitmq_global_metrics_collector.erl", "src/rabbit_prometheus_app.erl", "src/rabbit_prometheus_dispatcher.erl", "src/rabbit_prometheus_handler.erl"],
    )
