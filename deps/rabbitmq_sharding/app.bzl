load("@rules_erlang//:erlang_bytecode2.bzl", "erlang_bytecode")
load("@rules_erlang//:filegroup.bzl", "filegroup")

def all_beam_files():
    filegroup(
        name = "beam_files",
        srcs = ["ebin/rabbit_sharding_exchange_decorator.beam", "ebin/rabbit_sharding_exchange_type_modulus_hash.beam", "ebin/rabbit_sharding_interceptor.beam", "ebin/rabbit_sharding_policy_validator.beam", "ebin/rabbit_sharding_shard.beam", "ebin/rabbit_sharding_util.beam"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_sharding_exchange_decorator_beam",
        srcs = ["src/rabbit_sharding_exchange_decorator.erl"],
        outs = ["ebin/rabbit_sharding_exchange_decorator.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_sharding_exchange_type_modulus_hash_beam",
        srcs = ["src/rabbit_sharding_exchange_type_modulus_hash.erl"],
        outs = ["ebin/rabbit_sharding_exchange_type_modulus_hash.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_sharding_interceptor_beam",
        srcs = ["src/rabbit_sharding_interceptor.erl"],
        outs = ["ebin/rabbit_sharding_interceptor.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_sharding_policy_validator_beam",
        srcs = ["src/rabbit_sharding_policy_validator.erl"],
        outs = ["ebin/rabbit_sharding_policy_validator.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_sharding_shard_beam",
        srcs = ["src/rabbit_sharding_shard.erl"],
        outs = ["ebin/rabbit_sharding_shard.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "ebin_rabbit_sharding_util_beam",
        srcs = ["src/rabbit_sharding_util.erl"],
        outs = ["ebin/rabbit_sharding_util.beam"],
        erlc_opts = "//:erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_test_beam_files():
    filegroup(
        name = "test_beam_files",
        testonly = True,
        srcs = ["test/rabbit_sharding_exchange_decorator.beam", "test/rabbit_sharding_exchange_type_modulus_hash.beam", "test/rabbit_sharding_interceptor.beam", "test/rabbit_sharding_policy_validator.beam", "test/rabbit_sharding_shard.beam", "test/rabbit_sharding_util.beam"],
    )
    erlang_bytecode(
        name = "test_rabbit_sharding_exchange_decorator_beam",
        testonly = True,
        srcs = ["src/rabbit_sharding_exchange_decorator.erl"],
        outs = ["test/rabbit_sharding_exchange_decorator.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_sharding_exchange_type_modulus_hash_beam",
        testonly = True,
        srcs = ["src/rabbit_sharding_exchange_type_modulus_hash.erl"],
        outs = ["test/rabbit_sharding_exchange_type_modulus_hash.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_sharding_interceptor_beam",
        testonly = True,
        srcs = ["src/rabbit_sharding_interceptor.erl"],
        outs = ["test/rabbit_sharding_interceptor.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit:erlang_app", "//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_sharding_policy_validator_beam",
        testonly = True,
        srcs = ["src/rabbit_sharding_policy_validator.erl"],
        outs = ["test/rabbit_sharding_policy_validator.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_sharding_shard_beam",
        testonly = True,
        srcs = ["src/rabbit_sharding_shard.erl"],
        outs = ["test/rabbit_sharding_shard.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )
    erlang_bytecode(
        name = "test_rabbit_sharding_util_beam",
        testonly = True,
        srcs = ["src/rabbit_sharding_util.erl"],
        outs = ["test/rabbit_sharding_util.beam"],
        erlc_opts = "//:test_erlc_opts",
        deps = ["//deps/rabbit_common:erlang_app"],
    )

def all_srcs():
    filegroup(
        name = "all_srcs",
        srcs = ["src/rabbit_sharding_exchange_decorator.erl", "src/rabbit_sharding_exchange_type_modulus_hash.erl", "src/rabbit_sharding_interceptor.erl", "src/rabbit_sharding_policy_validator.erl", "src/rabbit_sharding_shard.erl", "src/rabbit_sharding_util.erl"],
    )
