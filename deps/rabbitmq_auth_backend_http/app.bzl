load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_auth_backend_http.beam", "ebin/rabbit_auth_backend_http_app.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_backend_http_app_beam",
        srcs = ["src/rabbit_auth_backend_http_app.erl"],
        outs = ["ebin/rabbit_auth_backend_http_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_backend_http_beam",
        srcs = ["src/rabbit_auth_backend_http.erl"],
        outs = ["ebin/rabbit_auth_backend_http.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_auth_backend_http.beam", "test/rabbit_auth_backend_http_app.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_auth_backend_http_app_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_backend_http_app.erl"],
        outs = ["test/rabbit_auth_backend_http_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_auth_backend_http_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_backend_http.erl"],
        outs = ["test/rabbit_auth_backend_http.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/rabbit_auth_backend_http.erl", "src/rabbit_auth_backend_http_app.erl"],
    )
