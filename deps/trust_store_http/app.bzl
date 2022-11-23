load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/trust_store_http.beam", "ebin/trust_store_http_app.beam", "ebin/trust_store_http_sup.beam", "ebin/trust_store_invalid_handler.beam", "ebin/trust_store_list_handler.beam"],
    )
    erlang_bytecode(
        name = "ebin_trust_store_http_app_beam",
        srcs = ["src/trust_store_http_app.erl"],
        outs = ["ebin/trust_store_http_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_trust_store_http_beam",
        srcs = ["src/trust_store_http.erl"],
        outs = ["ebin/trust_store_http.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_trust_store_http_sup_beam",
        srcs = ["src/trust_store_http_sup.erl"],
        outs = ["ebin/trust_store_http_sup.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_trust_store_invalid_handler_beam",
        srcs = ["src/trust_store_invalid_handler.erl"],
        outs = ["ebin/trust_store_invalid_handler.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_trust_store_list_handler_beam",
        srcs = ["src/trust_store_list_handler.erl"],
        outs = ["ebin/trust_store_list_handler.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/trust_store_http.erl", "src/trust_store_http_app.erl", "src/trust_store_http_sup.erl", "src/trust_store_invalid_handler.erl", "src/trust_store_list_handler.erl"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/trust_store_http.beam", "test/trust_store_http_app.beam", "test/trust_store_http_sup.beam", "test/trust_store_invalid_handler.beam", "test/trust_store_list_handler.beam"],
    )
    erlang_bytecode(
        name = "test_trust_store_http_app_beam",
        testonly = True,
        srcs = ["src/trust_store_http_app.erl"],
        outs = ["test/trust_store_http_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_trust_store_http_beam",
        testonly = True,
        srcs = ["src/trust_store_http.erl"],
        outs = ["test/trust_store_http.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_trust_store_http_sup_beam",
        testonly = True,
        srcs = ["src/trust_store_http_sup.erl"],
        outs = ["test/trust_store_http_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_trust_store_invalid_handler_beam",
        testonly = True,
        srcs = ["src/trust_store_invalid_handler.erl"],
        outs = ["test/trust_store_invalid_handler.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )
    erlang_bytecode(
        name = "test_trust_store_list_handler_beam",
        testonly = True,
        srcs = ["src/trust_store_list_handler.erl"],
        outs = ["test/trust_store_list_handler.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["@cowboy//:erlang_app"],
    )
