# Performance monitoring

Performance monitoring is done via the RISC-V HPM interface.
The counters are implemented in `HartPmu`, alongside appropriate events.

## Configuration

No programmable HPM counters are implemented by default.
Set `Parameters.numProgrammableHpmCounters` or `EmitCore --numProgrammableHpmCounters=N` to select 0-29
contiguous slots. Each `mhpmevent` selects one event from the table below.

## Memory events

Memory events are disabled by default. Set `Parameters.enableMemoryCounters = true`
or `EmitCore --enableMemoryCounters=true` to enable.

| ID | Event | Increment |
| --- | --- | --- |
| 0 | Disabled | 0 |
| 1 | LSU requests | # memory instructions admitted to LSU queue (i.e. not including `FENCEI`, which is not enqueued) |
| 2 | LSU stall cycles | One per cycle when next eligible load/store cannot be enqueued due to backpressure |
| 3 | DTCM read bytes | Always the row size even for subword reads (matches behavior) |
| 4 | DTCM write bytes | Enabled bytes per accepted write according to mask |
| 5 | External data-bus wait cycles | One per pending external request cycle with `ready=0` |
| 6 | External data-bus read bytes | Requested bytes per completed read |
| 7 | External data-bus write bytes | Enabled bytes per completed write according to mask |
| 8 | LSU bus transfers | # LSU transactions accepted by the bus, not necessarily complete |
| 9 | LSU bus wait cycles | Time taken for LSU bus to complete the transaction |
| 10 | LSU backlog with idle bus | LSU internal stall, when the the queue is non-empty but the bus is idle, i.e. LSU performing internal processing |

### LSU stall cycles

Event 2 increments at most one per cycle when a load/store is next in dispatch order,
all other eligibility checks pass (scoreboards, retirement-buffer space, control
and vector checks), and the only reason it cannot continue is that there is no queue entry
after older memory instructions.

### Counting boundaries

- Requests count once per instruction, including later-flushed requests. `FENCE.I` and `WFI` flush operations are excluded.
- Bus transfers count on `fire` (`valid && ready`).
- External events exclude internal CSRs and include MMIO and error completions.
- Wait cycles exclude completion and can overlap execution or LSU stalls.
- LSU bus events cover all LSU destinations (ibus, dbus, ebus)
- Backlog counts a nonempty LSU queue with no bus request, e.g.
  response processing, operand waits and writeback. It does not measure total LSU busy time.

## Usage

- Drain measured requests before freezing counters
- Read back `mcountinhibit` after changing it
- Keep counters frozen while reading RV32 halves

## Validation

```sh
bazel test \
  //hdl/chisel/src/coralnpu:coralnpu_hart_pmu_tests \
  //hdl/chisel/src/coralnpu:coralnpu_csr_tests \
  //hdl/chisel/src/coralnpu:coralnpu_decode_tests \
  //tests/cocotb:lsu_cycle_bench_test \
  //tests/cocotb:lsu_cycle_bench_counters_test --build_tests_only
```
