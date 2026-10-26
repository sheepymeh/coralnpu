package coralnpu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec

class HartPmuSpec extends AnyFreeSpec with ChiselSim {
  for (xlen <- Seq(32, 64)) {
    s"RV$xlen PMU retirement forwarding and reset" in {
      val p = new Parameters(xlen = xlen)
      simulate(new HartPmu(p)) { dut =>
        dut.io.csr.request.valid.poke(false.B)
        dut.io.csr.request.address.poke(0.U)
        dut.io.csr.request.op.poke(CsrOp.CSRRW)
        dut.io.csr.request.operand.poke(0.U)
        dut.io.csr.request.write.poke(false.B)
        dut.io.events.nRetired.poke(0.U)
        dut.clock.step()

        // Writes commit directly at PMU boundary
        def write_csr(address: CsrAddress.Type, value: BigInt): Unit = {
          dut.io.csr.request.valid.poke(true.B)
          dut.io.csr.request.address.poke(address.litValue.U)
          dut.io.csr.request.op.poke(CsrOp.CSRRW)
          dut.io.csr.request.operand.poke(value.U)
          dut.io.csr.request.write.poke(true.B)
          dut.clock.step()
          dut.io.csr.request.write.poke(false.B)
        }

        // A counter write wins over the concurrent cycle increment
        write_csr(CsrAddress.MCYCLE, 99)
        dut.io.csr.status.cycle.expect(99.U)
        dut.clock.step()
        dut.io.csr.status.cycle.expect(100.U)
        write_csr(CsrAddress.MCOUNTINHIBIT, 1)

        // Counter writes suppress retirement of the writing instruction
        write_csr(CsrAddress.MHPMCOUNTER3, 99)
        write_csr(CsrAddress.MINSTRET, 64)
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
          write_csr(CsrAddress.MINSTRETH, 0)
          write_csr(CsrAddress.MINSTRET, BigInt("ffffffff", 16))
          dut.clock.step()
          dut.io.events.nRetired.poke(1.U)
          write_csr(CsrAddress.MINSTRET, 7)
          dut.io.csr.status.instret.expect(((BigInt(1) << 32) | 7).U)
          dut.clock.step()
          dut.io.csr.status.instret.expect(((BigInt(1) << 32) | 7).U)

          dut.io.events.nRetired.poke(2.U)
          write_csr(CsrAddress.MINSTRETH, 3)
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
