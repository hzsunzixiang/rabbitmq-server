load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_auth_backend_ldap.beam", "ebin/rabbit_auth_backend_ldap_app.beam", "ebin/rabbit_auth_backend_ldap_util.beam", "ebin/rabbit_log_ldap.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_backend_ldap_app_beam",
        srcs = ["src/rabbit_auth_backend_ldap_app.erl"],
        outs = ["ebin/rabbit_auth_backend_ldap_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_backend_ldap_beam",
        srcs = ["src/rabbit_auth_backend_ldap.erl"],
        outs = ["ebin/rabbit_auth_backend_ldap.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_auth_backend_ldap_util_beam",
        srcs = ["src/rabbit_auth_backend_ldap_util.erl"],
        outs = ["ebin/rabbit_auth_backend_ldap_util.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbit_log_ldap_beam",
        srcs = ["src/rabbit_log_ldap.erl"],
        outs = ["ebin/rabbit_log_ldap.beam"],
        hdrs = ["include/logging.hrl"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_auth_backend_ldap.beam", "test/rabbit_auth_backend_ldap_app.beam", "test/rabbit_auth_backend_ldap_util.beam", "test/rabbit_log_ldap.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_auth_backend_ldap_app_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_backend_ldap_app.erl"],
        outs = ["test/rabbit_auth_backend_ldap_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_auth_backend_ldap_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_backend_ldap.erl"],
        outs = ["test/rabbit_auth_backend_ldap.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_auth_backend_ldap_util_beam",
        testonly = True,
        srcs = ["src/rabbit_auth_backend_ldap_util.erl"],
        outs = ["test/rabbit_auth_backend_ldap_util.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbit_log_ldap_beam",
        testonly = True,
        srcs = ["src/rabbit_log_ldap.erl"],
        outs = ["test/rabbit_log_ldap.beam"],
        hdrs = ["include/logging.hrl"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/logging.hrl", "src/rabbit_auth_backend_ldap.erl", "src/rabbit_auth_backend_ldap_app.erl", "src/rabbit_auth_backend_ldap_util.erl", "src/rabbit_log_ldap.erl"],
    )
