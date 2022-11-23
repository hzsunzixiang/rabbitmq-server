load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_web_stomp_examples_app.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_web_stomp_examples_app_beam",
        srcs = ["src/rabbit_web_stomp_examples_app.erl"],
        outs = ["ebin/rabbit_web_stomp_examples_app.beam"],
        erlc_opts = "//:erlc_opts",
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/rabbit_web_stomp_examples_app.erl"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_web_stomp_examples_app.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_web_stomp_examples_app_beam",
        testonly = True,
        srcs = ["src/rabbit_web_stomp_examples_app.erl"],
        outs = ["test/rabbit_web_stomp_examples_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
