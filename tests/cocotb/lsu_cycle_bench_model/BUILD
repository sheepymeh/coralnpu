load("//hdl/chisel/src/coralnpu:flags.bzl", "RVV_CORE_MINI_AXI_COMMON_GEN_FLAGS", "RVV_CORE_MINI_AXI_VOPTS")
load("//rules:chisel.bzl", "chisel_cc_library")
load("//rules:coco_tb.bzl", "verilator_cocotb_model")
load("//tests/cocotb:build_defs.bzl", "VERILATOR_BUILD_ARGS")

package(default_visibility = ["//visibility:public"])

chisel_cc_library(
    name = "core",
    chisel_lib = "//hdl/chisel/src/coralnpu:coralnpu",
    emit_class = "coralnpu.EmitCore",
    gen_flags = [
        "--moduleName=RvvCoreMini",
        "--itcmSizeKBytes=8",
        "--dtcmSizeKBytes=32",
        "--numProgrammableHpmCounters=4",
        "--enableMemoryCounters=true",
    ] + RVV_CORE_MINI_AXI_COMMON_GEN_FLAGS,
    module_name = "RvvCoreMiniAxi",
    vopts = RVV_CORE_MINI_AXI_VOPTS,
)

verilator_cocotb_model(
    name = "model",
    cflags = VERILATOR_BUILD_ARGS,
    hdl_toplevel = "RvvCoreMiniAxi",
    trace = False,
    verilog_source = ":RvvCoreMiniAxi.sv",
    vlt_tpl = "//coralnpu_test_utils:dispatch.vlt.tpl",
    deps = ["//hdl/verilog:sram_backdoor"],
)
