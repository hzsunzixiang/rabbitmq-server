load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_tracing_app.beam", "ebin/rabbit_tracing_consumer.beam", "ebin/rabbit_tracing_consumer_sup.beam", "ebin/rabbit_tracing_files.beam", "ebin/rabbit_tracing_mgmt.beam", "ebin/rabbit_tracing_sup.beam", "ebin/rabbit_tracing_traces.beam", "ebin/rabbit_tracing_util.beam", "ebin/rabbit_tracing_wm_file.beam", "ebin/rabbit_tracing_wm_files.beam", "ebin/rabbit_tracing_wm_trace.beam", "ebin/rabbit_tracing_wm_traces.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_tracing_app_beam",
        srcs = ["src/rabbit_tracing_app.erl"],
        outs = ["ebin/rabbit_tracing_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_tracing_consumer_beam",
        srcs = ["src/rabbit_tracing_consumer.erl"],
        outs = ["ebin/rabbit_tracing_consumer.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_tracing_consumer_sup_beam",
        srcs = ["src/rabbit_tracing_consumer_sup.erl"],
        outs = ["ebin/rabbit_tracing_consumer_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_tracing_files_beam",
        srcs = ["src/rabbit_tracing_files.erl"],
        outs = ["ebin/rabbit_tracing_files.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_tracing_mgmt_beam",
        srcs = ["src/rabbit_tracing_mgmt.erl"],
        outs = ["ebin/rabbit_tracing_mgmt.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_tracing_sup_beam",
        srcs = ["src/rabbit_tracing_sup.erl"],
        outs = ["ebin/rabbit_tracing_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_tracing_traces_beam",
        srcs = ["src/rabbit_tracing_traces.erl"],
        outs = ["ebin/rabbit_tracing_traces.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_tracing_util_beam",
        srcs = ["src/rabbit_tracing_util.erl"],
        outs = ["ebin/rabbit_tracing_util.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_tracing_wm_file_beam",
        srcs = ["src/rabbit_tracing_wm_file.erl"],
        outs = ["ebin/rabbit_tracing_wm_file.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_tracing_wm_files_beam",
        srcs = ["src/rabbit_tracing_wm_files.erl"],
        outs = ["ebin/rabbit_tracing_wm_files.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_tracing_wm_trace_beam",
        srcs = ["src/rabbit_tracing_wm_trace.erl"],
        outs = ["ebin/rabbit_tracing_wm_trace.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_tracing_wm_traces_beam",
        srcs = ["src/rabbit_tracing_wm_traces.erl"],
        outs = ["ebin/rabbit_tracing_wm_traces.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_tracing_app.beam", "test/rabbit_tracing_consumer.beam", "test/rabbit_tracing_consumer_sup.beam", "test/rabbit_tracing_files.beam", "test/rabbit_tracing_mgmt.beam", "test/rabbit_tracing_sup.beam", "test/rabbit_tracing_traces.beam", "test/rabbit_tracing_util.beam", "test/rabbit_tracing_wm_file.beam", "test/rabbit_tracing_wm_files.beam", "test/rabbit_tracing_wm_trace.beam", "test/rabbit_tracing_wm_traces.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_tracing_app_beam",
        testonly = True,
        srcs = ["src/rabbit_tracing_app.erl"],
        outs = ["test/rabbit_tracing_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_tracing_consumer_beam",
        testonly = True,
        srcs = ["src/rabbit_tracing_consumer.erl"],
        outs = ["test/rabbit_tracing_consumer.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/amqp_client:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_tracing_consumer_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_tracing_consumer_sup.erl"],
        outs = ["test/rabbit_tracing_consumer_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_tracing_files_beam",
        testonly = True,
        srcs = ["src/rabbit_tracing_files.erl"],
        outs = ["test/rabbit_tracing_files.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_tracing_mgmt_beam",
        testonly = True,
        srcs = ["src/rabbit_tracing_mgmt.erl"],
        outs = ["test/rabbit_tracing_mgmt.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_tracing_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_tracing_sup.erl"],
        outs = ["test/rabbit_tracing_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_tracing_traces_beam",
        testonly = True,
        srcs = ["src/rabbit_tracing_traces.erl"],
        outs = ["test/rabbit_tracing_traces.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_tracing_util_beam",
        testonly = True,
        srcs = ["src/rabbit_tracing_util.erl"],
        outs = ["test/rabbit_tracing_util.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_tracing_wm_file_beam",
        testonly = True,
        srcs = ["src/rabbit_tracing_wm_file.erl"],
        outs = ["test/rabbit_tracing_wm_file.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_tracing_wm_files_beam",
        testonly = True,
        srcs = ["src/rabbit_tracing_wm_files.erl"],
        outs = ["test/rabbit_tracing_wm_files.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_tracing_wm_trace_beam",
        testonly = True,
        srcs = ["src/rabbit_tracing_wm_trace.erl"],
        outs = ["test/rabbit_tracing_wm_trace.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_tracing_wm_traces_beam",
        testonly = True,
        srcs = ["src/rabbit_tracing_wm_traces.erl"],
        outs = ["test/rabbit_tracing_wm_traces.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbitmq_management_agent:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/rabbit_tracing_app.erl", "src/rabbit_tracing_consumer.erl", "src/rabbit_tracing_consumer_sup.erl", "src/rabbit_tracing_files.erl", "src/rabbit_tracing_mgmt.erl", "src/rabbit_tracing_sup.erl", "src/rabbit_tracing_traces.erl", "src/rabbit_tracing_util.erl", "src/rabbit_tracing_wm_file.erl", "src/rabbit_tracing_wm_files.erl", "src/rabbit_tracing_wm_trace.erl", "src/rabbit_tracing_wm_traces.erl"],
    )
