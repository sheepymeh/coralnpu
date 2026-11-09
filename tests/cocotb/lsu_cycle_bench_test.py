"""Measure independent LSU throughput separately from load dependency latency."""

import json
import os
import struct
from pathlib import Path

import cocotb
from bazel_tools.tools.python.runfiles import runfiles

from coralnpu_test_utils.sim_backends.verilator_test_fixture import VerilatorTestFixture

COUNTERS = (
    "cycles",
    "instructions",
    "dispatch_stalls",
    "bus_transfers",
    "bus_wait_cycles",
    "backlog_bus_idle_cycles",
)
CASES = (
    ("no_memory", 5, False),
    ("independent_loads", 0, False),
    ("pointer_chase", 1, False),
    ("independent_stores", 2, False),
    ("vector_loads", 3, False),
    ("vector_stores", 4, False),
    ("external_loads", 0, True),
)


@cocotb.test()
async def lsu_cycle_bench_test(dut):
    fixture = await VerilatorTestFixture.Create(dut, read_delay_cycles=8)
    r = runfiles.Create()
    elf = r.Rlocation("coralnpu_hw/tests/cocotb/lsu_cycle_bench.elf")
    enabled = os.environ.get("EXPECT_MEMORY_COUNTERS", "0") == "1"
    results = {}
    dut._log.info("Benchmark LSU using performance counters")

    for name, bench_id, external in CASES:
        await fixture.load_elf_and_lookup_symbols(
            elf,
            [
                "g_selected_bench",
                "g_buffer",
                "g_memory_counters_enabled",
                "g_results",
                "g_scratch",
            ],
        )
        await fixture.write_word("g_selected_bench", bench_id)
        if external:
            await fixture.write_word("g_buffer", 0x20000000)
            await fixture.write(0x20000000, bytes(128))
        await fixture.run_to_halt(timeout_cycles=20000)
        actual_enabled = int.from_bytes(
            bytes(await fixture.read_word("g_memory_counters_enabled")),
            "little"
        )
        assert actual_enabled == enabled

        values = struct.unpack(
            "<6Q", bytes(await fixture.read("g_results", size_bytes=48))
        )
        row = dict(zip(COUNTERS, values))
        results[name] = row
        dut._log.info(
            "%s: %s", name, ", ".join(str(value) for value in values)
        )
        assert row["cycles"] > 0 and row["instructions"] >= 32
        if enabled and name != "no_memory":
            assert row["bus_transfers"] == 32, (name, row)
            if external:
                assert row["bus_wait_cycles"] >= 32 * 8, row
            else:
                assert row["bus_wait_cycles"] == 0, (name, row)
        else:
            assert all(row[counter] == 0 for counter in COUNTERS[2:]), row

        if name in ("independent_stores", "vector_stores"):
            size = 32 if bench_id == 2 else 128
            expected = 42 if bench_id == 2 else 7
            data = bytes(await fixture.read("g_scratch", size_bytes=size))
            words = struct.unpack(f"<{size // 4}I", data)
            assert all(value == expected for value in words), (name, words)

    output_dir = os.environ.get("TEST_UNDECLARED_OUTPUTS_DIR")
    if output_dir:
        output_path = Path(output_dir, "lsu_cycle_bench.json")
        output_path.write_text(json.dumps(results, indent=2) + "\n")
