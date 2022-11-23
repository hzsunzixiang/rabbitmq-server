load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_event_exchange_decorator.beam", "ebin/rabbit_exchange_type_event.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_event_exchange_decorator_beam",
        srcs = ["src/rabbit_event_exchange_decorator.erl"],
        outs = ["ebin/rabbit_event_exchange_decorator.beam"],
        hdrs = ["include/rabbit_event_exchange.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_exchange_type_event_beam",
        srcs = ["src/rabbit_exchange_type_event.erl"],
        outs = ["ebin/rabbit_exchange_type_event.beam"],
        hdrs = ["include/rabbit_event_exchange.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_event_exchange_decorator.beam", "test/rabbit_exchange_type_event.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_event_exchange_decorator_beam",
        testonly = True,
        srcs = ["src/rabbit_event_exchange_decorator.erl"],
        outs = ["test/rabbit_event_exchange_decorator.beam"],
        hdrs = ["include/rabbit_event_exchange.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_exchange_type_event_beam",
        testonly = True,
        srcs = ["src/rabbit_exchange_type_event.erl"],
        outs = ["test/rabbit_exchange_type_event.beam"],
        hdrs = ["include/rabbit_event_exchange.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/rabbit_event_exchange.hrl", "src/rabbit_event_exchange_decorator.erl", "src/rabbit_exchange_type_event.erl"],
    )
