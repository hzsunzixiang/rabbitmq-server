load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_trust_store.beam", "ebin/rabbit_trust_store_app.beam", "ebin/rabbit_trust_store_certificate_provider.beam", "ebin/rabbit_trust_store_file_provider.beam", "ebin/rabbit_trust_store_http_provider.beam", "ebin/rabbit_trust_store_sup.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_trust_store_app_beam",
        srcs = ["src/rabbit_trust_store_app.erl"],
        outs = ["ebin/rabbit_trust_store_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_trust_store_beam",
        srcs = ["src/rabbit_trust_store.erl"],
        outs = ["ebin/rabbit_trust_store.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_trust_store_certificate_provider_beam",
        srcs = ["src/rabbit_trust_store_certificate_provider.erl"],
        outs = ["ebin/rabbit_trust_store_certificate_provider.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_trust_store_file_provider_beam",
        srcs = ["src/rabbit_trust_store_file_provider.erl"],
        outs = ["ebin/rabbit_trust_store_file_provider.beam"],
        beam = ["ebin/rabbit_trust_store_certificate_provider.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_trust_store_http_provider_beam",
        srcs = ["src/rabbit_trust_store_http_provider.erl"],
        outs = ["ebin/rabbit_trust_store_http_provider.beam"],
        beam = ["ebin/rabbit_trust_store_certificate_provider.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_trust_store_sup_beam",
        srcs = ["src/rabbit_trust_store_sup.erl"],
        outs = ["ebin/rabbit_trust_store_sup.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_trust_store.beam", "test/rabbit_trust_store_app.beam", "test/rabbit_trust_store_certificate_provider.beam", "test/rabbit_trust_store_file_provider.beam", "test/rabbit_trust_store_http_provider.beam", "test/rabbit_trust_store_sup.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_trust_store_app_beam",
        testonly = True,
        srcs = ["src/rabbit_trust_store_app.erl"],
        outs = ["test/rabbit_trust_store_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_trust_store_beam",
        testonly = True,
        srcs = ["src/rabbit_trust_store.erl"],
        outs = ["test/rabbit_trust_store.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_trust_store_certificate_provider_beam",
        testonly = True,
        srcs = ["src/rabbit_trust_store_certificate_provider.erl"],
        outs = ["test/rabbit_trust_store_certificate_provider.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_trust_store_file_provider_beam",
        testonly = True,
        srcs = ["src/rabbit_trust_store_file_provider.erl"],
        outs = ["test/rabbit_trust_store_file_provider.beam"],
        beam = ["ebin/rabbit_trust_store_certificate_provider.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_trust_store_http_provider_beam",
        testonly = True,
        srcs = ["src/rabbit_trust_store_http_provider.erl"],
        outs = ["test/rabbit_trust_store_http_provider.beam"],
        beam = ["ebin/rabbit_trust_store_certificate_provider.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_trust_store_sup_beam",
        testonly = True,
        srcs = ["src/rabbit_trust_store_sup.erl"],
        outs = ["test/rabbit_trust_store_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/rabbit_trust_store.erl", "src/rabbit_trust_store_app.erl", "src/rabbit_trust_store_certificate_provider.erl", "src/rabbit_trust_store_file_provider.erl", "src/rabbit_trust_store_http_provider.erl", "src/rabbit_trust_store_sup.erl"],
    )
