load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbitmq_aws.beam", "ebin/rabbitmq_aws_app.beam", "ebin/rabbitmq_aws_config.beam", "ebin/rabbitmq_aws_json.beam", "ebin/rabbitmq_aws_sign.beam", "ebin/rabbitmq_aws_sup.beam", "ebin/rabbitmq_aws_urilib.beam", "ebin/rabbitmq_aws_xml.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_aws_app_beam",
        srcs = ["src/rabbitmq_aws_app.erl"],
        outs = ["ebin/rabbitmq_aws_app.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_aws_beam",
        srcs = ["src/rabbitmq_aws.erl"],
        outs = ["ebin/rabbitmq_aws.beam"],
        hdrs = ["include/rabbitmq_aws.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_aws_config_beam",
        srcs = ["src/rabbitmq_aws_config.erl"],
        outs = ["ebin/rabbitmq_aws_config.beam"],
        hdrs = ["include/rabbitmq_aws.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_aws_json_beam",
        srcs = ["src/rabbitmq_aws_json.erl"],
        outs = ["ebin/rabbitmq_aws_json.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_aws_sign_beam",
        srcs = ["src/rabbitmq_aws_sign.erl"],
        outs = ["ebin/rabbitmq_aws_sign.beam"],
        hdrs = ["include/rabbitmq_aws.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_aws_sup_beam",
        srcs = ["src/rabbitmq_aws_sup.erl"],
        outs = ["ebin/rabbitmq_aws_sup.beam"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_aws_urilib_beam",
        srcs = ["src/rabbitmq_aws_urilib.erl"],
        outs = ["ebin/rabbitmq_aws_urilib.beam"],
        hdrs = ["include/rabbitmq_aws.hrl"],
        erlc_opts = "//:erlc_opts",
    )
    erlang_bytecode(
        name = "ebin_rabbitmq_aws_xml_beam",
        srcs = ["src/rabbitmq_aws_xml.erl"],
        outs = ["ebin/rabbitmq_aws_xml.beam"],
        erlc_opts = "//:erlc_opts",
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbitmq_aws.beam", "test/rabbitmq_aws_app.beam", "test/rabbitmq_aws_config.beam", "test/rabbitmq_aws_json.beam", "test/rabbitmq_aws_sign.beam", "test/rabbitmq_aws_sup.beam", "test/rabbitmq_aws_urilib.beam", "test/rabbitmq_aws_xml.beam"],
    )
    erlang_bytecode(
        name = "test_rabbitmq_aws_app_beam",
        testonly = True,
        srcs = ["src/rabbitmq_aws_app.erl"],
        outs = ["test/rabbitmq_aws_app.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbitmq_aws_beam",
        testonly = True,
        srcs = ["src/rabbitmq_aws.erl"],
        outs = ["test/rabbitmq_aws.beam"],
        hdrs = ["include/rabbitmq_aws.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbitmq_aws_config_beam",
        testonly = True,
        srcs = ["src/rabbitmq_aws_config.erl"],
        outs = ["test/rabbitmq_aws_config.beam"],
        hdrs = ["include/rabbitmq_aws.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbitmq_aws_json_beam",
        testonly = True,
        srcs = ["src/rabbitmq_aws_json.erl"],
        outs = ["test/rabbitmq_aws_json.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbitmq_aws_sign_beam",
        testonly = True,
        srcs = ["src/rabbitmq_aws_sign.erl"],
        outs = ["test/rabbitmq_aws_sign.beam"],
        hdrs = ["include/rabbitmq_aws.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbitmq_aws_sup_beam",
        testonly = True,
        srcs = ["src/rabbitmq_aws_sup.erl"],
        outs = ["test/rabbitmq_aws_sup.beam"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbitmq_aws_urilib_beam",
        testonly = True,
        srcs = ["src/rabbitmq_aws_urilib.erl"],
        outs = ["test/rabbitmq_aws_urilib.beam"],
        hdrs = ["include/rabbitmq_aws.hrl"],
        erlc_opts = "//:test_erlc_opts",
    )
    erlang_bytecode(
        name = "test_rabbitmq_aws_xml_beam",
        testonly = True,
        srcs = ["src/rabbitmq_aws_xml.erl"],
        outs = ["test/rabbitmq_aws_xml.beam"],
        erlc_opts = "//:test_erlc_opts",
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["include/rabbitmq_aws.hrl", "src/rabbitmq_aws.erl", "src/rabbitmq_aws_app.erl", "src/rabbitmq_aws_config.erl", "src/rabbitmq_aws_json.erl", "src/rabbitmq_aws_sign.erl", "src/rabbitmq_aws_sup.erl", "src/rabbitmq_aws_urilib.erl", "src/rabbitmq_aws_xml.erl"],
    )
