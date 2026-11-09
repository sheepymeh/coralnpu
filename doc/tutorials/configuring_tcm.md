# Configuring ITCM and DTCM size

CoralNPU defaults to 8 KiB ITCM and 32 KiB DTCM, or 1 MiB each in the `highmem` config.
This guide is for non-standard ITCM/DTCM sizes.

## Generate the core

Pass `--itcmSizeKBytes` and `--dtcmSizeKBytes` to `EmitCore`.

You can add a target to `hdl/chisel/src/coralnpu/prod/BUILD`,
e.g. for a core with 16 KiB ITCM and 256 KiB DTCM:

```starlark
chisel_cc_library(
    name = "rvv_core_mini_itcm16kb_dtcm256kb_axi_prod_cc_library",
    chisel_lib = "//hdl/chisel/src/coralnpu:coralnpu",
    emit_class = "coralnpu.EmitCore",
    extra_outs = [
        "VRvvCoreMini_ITCM16KB_DTCM256KBAxi_parameters.h",
        "RvvCoreMini_ITCM16KB_DTCM256KBAxi.zip",
    ],
    gen_flags = [
        "--moduleName=RvvCoreMini",
        "--itcmSizeKBytes=16",
        "--dtcmSizeKBytes=256",
    ] + RVV_CORE_MINI_AXI_COMMON_GEN_FLAGS,
    module_name = "RvvCoreMini_ITCM16KB_DTCM256KBAxi",
    vopts = RVV_CORE_MINI_AXI_VOPTS,
)
```

Generate the RTL and its resource archive:

```sh
bazel build \
  //hdl/chisel/src/coralnpu/prod:RvvCoreMini_ITCM16KB_DTCM256KBAxi.sv \
  //hdl/chisel/src/coralnpu/prod:RvvCoreMini_ITCM16KB_DTCM256KBAxi.zip
```

Custom sizes change the generated module name. Update the integration to use
both the new module name and its matching RTL and resource archive.

## Update the memory map

The core uses different base addresses for default and custom sizes:

| Configuration | ITCM base | DTCM base | Core CSR base |
| --- | --- | --- | --- |
| 8 KiB ITCM / 32 KiB DTCM | `0x00000000` | `0x00010000` | `0x00030000` |
| Other size combinations | `0x00000000` | `0x00100000` | `0x00200000` |

`highmem` uses the custom-size layout. Make sure the selected sizes fit
without overlapping other regions. Update the surrounding system's
address map, loader and any software that uses these addresses.

## Build matching firmware

Set the same sizes on the `coralnpu_v2_binary` target. For the 16/256 example:

```starlark
coralnpu_v2_binary(
    name = "firmware",
    srcs = ["firmware.cc"],
    itcm_size_kbytes = 16,
    dtcm_size_kbytes = 256,
)
```

The rule generates a matching linker script unless a custom one is supplied.
Choose stack and heap sizes appropriate for the program,. The default linker
places both code and read-only data in ITCM.

Check the linked ELF's load addresses and section sizes:

```sh
readelf -lW firmware.elf
size -A firmware.elf
```
