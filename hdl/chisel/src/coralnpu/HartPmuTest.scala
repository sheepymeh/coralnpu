package coralnpu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec

class HartPmuSpec extends AnyFreeSpec with ChiselSim {
  private def init_dut(dut: HartPmu): Unit = {
    dut.io.csr.request.valid.poke(false.B)
    dut.io.csr.request.address.poke(0.U)
    dut.io.csr.request.op.poke(CsrOp.CSRRW)
    dut.io.csr.request.operand.poke(0.U)
    dut.io.csr.request.write.poke(false.B)
    dut.io.events.nRetired.poke(0.U)
    dut.io.events.memory.foreach { events =>
      events.lsuRequests.poke(0.U)
      events.lsuStallCycles.poke(false.B)
      events.dtcmReadBytes.poke(0.U)
      events.dtcmWriteBytes.poke(0.U)
      events.externalWaitCycles.poke(false.B)
      events.externalReadBytes.poke(0.U)
      events.externalWriteBytes.poke(0.U)
      events.lsuBusTransfers.poke(false.B)
      events.lsuBusWaitCycles.poke(false.B)
      events.lsuBacklogBusIdleCycles.poke(false.B)
    }
    dut.clock.step()
  }

  // Writes commit directly at PMU boundary
  private def write_csr(
    dut: HartPmu,
    address: CsrAddress.Type,
    value: BigInt
  ): Unit = {
    dut.io.csr.request.valid.poke(true.B)
    dut.io.csr.request.address.poke(address.litValue.U)
    dut.io.csr.request.op.poke(CsrOp.CSRRW)
    dut.io.csr.request.operand.poke(value.U)
    dut.io.csr.request.write.poke(true.B)
    dut.clock.step()
    dut.io.csr.request.write.poke(false.B)
  }

  "Memory events and PMU update ordering" in {
    val p = new Parameters
    p.numProgrammableHpmCounters = 4
    p.enableMemoryCounters = true
    simulate(new HartPmu(p)) { dut =>
      init_dut(dut)

      def read_csr(address: CsrAddress.Type): BigInt = {
        dut.io.csr.request.address.poke(address.litValue.U)
        dut.io.csr.response.readData.peek().litValue
      }

      def hpm: BigInt = {
        val low = read_csr(CsrAddress.MHPMCOUNTER3)
        low | (read_csr(CsrAddress.MHPMCOUNTER3H) << 32)
      }

      // Drive all sources to check selector isolation and increment sizes
      val events = dut.io.events.memory.get
      events.lsuRequests.poke(3.U)
      events.lsuStallCycles.poke(true.B)
      events.dtcmReadBytes.poke(p.lsuDataBytes.U)
      events.dtcmWriteBytes.poke(17.U)
      events.externalWaitCycles.poke(true.B)
      events.externalReadBytes.poke(5.U)
      events.externalWriteBytes.poke(9.U)
      events.lsuBusTransfers.poke(true.B)
      events.lsuBusWaitCycles.poke(true.B)
      events.lsuBacklogBusIdleCycles.poke(true.B)
      val inhibit_all = 0x7d
      val enable_hpm3 = 0x75
      write_csr(dut, CsrAddress.MCOUNTINHIBIT, inhibit_all)

      // Disabled and unsupported selectors must not accumulate active events
      val cases = Seq(
        MemoryHpmEvent.NONE                        -> 0,
        MemoryHpmEvent.LSU_REQUESTS                -> 3,
        MemoryHpmEvent.LSU_STALL_CYCLES            -> 1,
        MemoryHpmEvent.DTCM_READ_BYTES             -> p.lsuDataBytes,
        MemoryHpmEvent.DTCM_WRITE_BYTES            -> 17,
        MemoryHpmEvent.EXTERNAL_WAIT_CYCLES        -> 1,
        MemoryHpmEvent.EXTERNAL_READ_BYTES         -> 5,
        MemoryHpmEvent.EXTERNAL_WRITE_BYTES        -> 9,
        MemoryHpmEvent.LSU_BUS_TRANSFERS           -> 1,
        MemoryHpmEvent.LSU_BUS_WAIT_CYCLES         -> 1,
        MemoryHpmEvent.LSU_BACKLOG_BUS_IDLE_CYCLES -> 1,
        0xff                                       -> 0
      )
      for ((id, amount) <- cases) {
        write_csr(dut, CsrAddress.MHPMEVENT3, id)
        assert(read_csr(CsrAddress.MHPMEVENT3) == (if (amount == 0) 0 else id))
        write_csr(dut, CsrAddress.MHPMCOUNTER3, 0)
        write_csr(dut, CsrAddress.MCOUNTINHIBIT, enable_hpm3)
        dut.clock.step(3)
        write_csr(dut, CsrAddress.MCOUNTINHIBIT, inhibit_all)
        assert(hpm == 4 * amount)
      }

      // Enable and selector writes take effect after their commit edge
      write_csr(dut, CsrAddress.MHPMEVENT3, MemoryHpmEvent.LSU_REQUESTS)
      write_csr(dut, CsrAddress.MCOUNTINHIBIT, enable_hpm3)
      assert(hpm == 0)

      write_csr(dut, CsrAddress.MHPMEVENT3, MemoryHpmEvent.LSU_STALL_CYCLES)
      assert(hpm == 3)
      dut.clock.step()
      assert(hpm == 4)
      write_csr(dut, CsrAddress.MCOUNTINHIBIT, inhibit_all)
      assert(hpm == 5)
      dut.clock.step(3)
      assert(hpm == 5)

      // Software counter writes win over simultaneous events
      write_csr(dut, CsrAddress.MCOUNTINHIBIT, enable_hpm3)
      write_csr(dut, CsrAddress.MHPMCOUNTER3, 99)
      assert(hpm == 99)
      dut.clock.step()
      assert(hpm == 100)
      write_csr(dut, CsrAddress.MCOUNTINHIBIT, inhibit_all)

      // Exercise RV32 carry and full 64-bit wrap with one increment
      val initial = BigInt("fffffffffffffffe", 16)
      write_csr(dut, CsrAddress.MHPMCOUNTER3, initial & 0xffffffffL)
      write_csr(dut, CsrAddress.MHPMCOUNTER3H, initial >> 32)
      write_csr(dut, CsrAddress.MHPMEVENT3, MemoryHpmEvent.LSU_REQUESTS)
      write_csr(dut, CsrAddress.MCOUNTINHIBIT, enable_hpm3)
      dut.clock.step()
      events.lsuRequests.poke(0.U)
      assert(hpm == 1)
      write_csr(dut, CsrAddress.MCOUNTINHIBIT, inhibit_all)

      // Reset clears event selection as well as accumulated count
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      assert(read_csr(CsrAddress.MHPMEVENT3) == 0)
      assert(hpm == 0)
    }
  }

  "Disabling memory events preserves generic counters" in {
    val p = new Parameters
    p.numProgrammableHpmCounters = 1
    p.enableMemoryCounters = false
    assert(!p.augmentId(1).enableMemoryCounters)
    assert(
      EmitParametersHeader(p)
        .replaceAll("\\s+", " ")
        .contains("#define KP_enableMemoryCounters false")
    )
    simulate(new HartPmu(p)) { dut =>
      init_dut(dut)
      assert(dut.io.events.memory.isEmpty)

      // Every memory selector legalizes to disabled event 0
      for (event <- 1 to MemoryHpmEvent.LSU_BACKLOG_BUS_IDLE_CYCLES) {
        write_csr(dut, CsrAddress.MHPMEVENT3, event)
        dut.io.csr.response.readData.expect(0.U)
      }

      // HPM storage remains software-writable without event sources
      write_csr(dut, CsrAddress.MHPMCOUNTER3, 99)
      val before = dut.io.csr.status.cycle.peek().litValue
      dut.io.events.nRetired.poke(2.U)
      dut.clock.step(3)
      dut.io.csr.response.readData.expect(99.U)
      dut.io.csr.status.cycle.expect((before + 3).U)
      dut.io.csr.status.instret.expect(6.U)
    }
  }

  for (xlen <- Seq(32, 64)) {
    s"RV$xlen PMU retirement forwarding and reset" in {
      val p = new Parameters(xlen = xlen)
      p.numProgrammableHpmCounters = 1
      simulate(new HartPmu(p)) { dut =>
        init_dut(dut)

        // A counter write wins over the concurrent cycle increment
        write_csr(dut, CsrAddress.MCYCLE, 99)
        dut.io.csr.status.cycle.expect(99.U)
        dut.clock.step()
        dut.io.csr.status.cycle.expect(100.U)
        write_csr(dut, CsrAddress.MCOUNTINHIBIT, 1)

        // Counter writes suppress retirement of the writing instruction
        write_csr(dut, CsrAddress.MHPMCOUNTER3, 99)
        write_csr(dut, CsrAddress.MINSTRET, 64)
        dut.io.events.nRetired.poke(1.U)
        dut.clock.step()
        dut.io.csr.status.instret.expect(64.U)

        // RMW uses forwarded retirement, host status still exposes stored count
        dut.io.events.nRetired.poke(2.U)
        dut.io.csr.request.address.poke(CsrAddress.MINSTRET.litValue.U)
        dut.io.csr.request.op.poke(CsrOp.CSRRS)
        dut.io.csr.request.operand.poke(8.U)
        dut.io.csr.request.write.poke(true.B)
        dut.io.csr.status.instret.expect(64.U)
        dut.io.csr.response.readData.expect(66.U)
        dut.io.csr.response.writeData.expect(74.U)
        dut.clock.step()
        dut.io.csr.status.instret.expect(74.U)
        dut.io.csr.request.write.poke(false.B)
        dut.io.events.nRetired.poke(1.U)
        dut.clock.step()
        dut.io.csr.status.instret.expect(74.U)

        if (xlen == 32) {
          // Partial writes preserve retirement from older instructions in untouched half
          dut.io.events.nRetired.poke(0.U)
          write_csr(dut, CsrAddress.MINSTRETH, 0)
          write_csr(dut, CsrAddress.MINSTRET, BigInt("ffffffff", 16))
          dut.clock.step()
          dut.io.events.nRetired.poke(1.U)
          write_csr(dut, CsrAddress.MINSTRET, 7)
          dut.io.csr.status.instret.expect(((BigInt(1) << 32) | 7).U)
          dut.clock.step()
          dut.io.csr.status.instret.expect(((BigInt(1) << 32) | 7).U)

          dut.io.events.nRetired.poke(2.U)
          write_csr(dut, CsrAddress.MINSTRETH, 3)
          dut.io.csr.status.instret.expect(((BigInt(3) << 32) | 9).U)
          dut.clock.step()
          dut.io.csr.status.instret.expect(((BigInt(3) << 32) | 9).U)
        }

        // Reset overrides writes and retirement, clearing delayed controls too
        dut.io.csr.request.write.poke(true.B)
        dut.io.events.nRetired.poke(3.U)
        dut.reset.poke(true.B)
        dut.clock.step()
        dut.io.csr.status.cycle.expect(0.U)
        dut.io.csr.status.instret.expect(0.U)
        dut.io.csr.request.address.poke(CsrAddress.MHPMCOUNTER3.litValue.U)
        dut.io.csr.response.readData.expect(0.U)
        dut.io.csr.request.write.poke(false.B)
        dut.io.events.nRetired.poke(1.U)
        dut.reset.poke(false.B)
        dut.clock.step()
        dut.io.csr.status.cycle.expect(1.U)
        dut.io.csr.status.instret.expect(1.U)
      }
    }
  }
}
