// Copyright 2026 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package coralnpu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DecodeInstructionSpec extends AnyFreeSpec with Matchers with ChiselSim {
  val p = new Parameters()

  class DecodeTester(p: Parameters) extends Module {
    val io = IO(new Bundle {
      val inst  = Input(UInt(32.W))
      val undef = Output(Bool())
    })

    val d = DecodeInstruction(p, pipeline = 0, addr = 0.U, op = io.inst, csrFrm = 0.U)
    io.undef := d.undef
  }

  "DecodeInstruction" - {
    "should decode all table instructions as valid (undef == false)" in {
      simulate(new DecodeTester(p)) { dut =>
        val tbl = DecodeInstruction.table(p)
        for ((name, pat) <- tbl) {
          dut.io.inst.poke(pat.value.U(32.W))
          dut.clock.step()
          assert(
            dut.io.undef.peek().litValue == 0,
            s"Instruction $name (0x${pat.value.toString(16)}) decoded as undef"
          )
        }
      }
    }

    "should decode invalid opcodes as undefined (undef == true)" in {
      simulate(new DecodeTester(p)) { dut =>
        val invalidWords = Seq(
          0x00000000L,         // All zeros
          0xffffffffL,         // All ones
          0x00000000L | 0x7bL, // Invalid custom opcode
          0x00000000L | 0x0bL, // Invalid opcode
          0x00000000L | 0x2bL  // Invalid opcode
        )
        for (word <- invalidWords) {
          dut.io.inst.poke(word.U(32.W))
          dut.clock.step()
          assert(
            dut.io.undef.peek().litValue == 1,
            s"Word 0x${word.toHexString} should decode as undef"
          )
        }
      }
    }
  }
}

object DispatchTestUtils {
  import chisel3.simulator.PeekPokeAPI._

  def initialize(dut: DispatchV2, p: Parameters): Unit = {
    dut.io.halted.poke(false.B)
    dut.io.lsuActive.poke(false.B)
    dut.io.scoreboard.regd.poke(0.U)
    dut.io.scoreboard.comb.poke(0.U)
    dut.io.branchTaken.poke(false.B)
    dut.io.interlock.poke(false.B)
    dut.io.lsuQueueCapacity.poke(p.instructionLanes.U)
    dut.io.retirement_buffer_nSpace.poke(p.retirementBufferSize.U)
    dut.io.retirement_buffer_empty.poke(true.B)
    dut.io.retirement_buffer_trap_pending.poke(false.B)
    dut.io.single_step.poke(false.B)
    dut.io.debug_mode.poke(false.B)
    for (i <- 0 until p.instructionLanes) {
      dut.io.inst(i).valid.poke(false.B)
      dut.io.inst(i).bits.addr.poke((0x100 + 4 * i).U)
      dut.io.inst(i).bits.inst.poke(0x13.U)
      dut.io.inst(i).bits.brchFwd.poke(false.B)
      dut.io.jalrTarget(i).data.poke(0.U)
      dut.io.lsu(i).ready.poke(true.B)
      dut.io.mlu(i).ready.poke(true.B)
      dut.io.dvu(i).ready.poke(true.B)
    }
    dut.io.rvv.foreach(_.foreach(_.ready.poke(true.B)))
    dut.io.rvvIdle.foreach(_.poke(true.B))
    dut.io.rvvQueueCapacity.foreach(_.poke(p.instructionLanes.U))
    dut.io.rvvState.foreach { state =>
      state.valid.poke(true.B)
      state.bits.vl.poke(4.U)
      state.bits.vstart.poke(0.U)
      state.bits.ma.poke(false.B)
      state.bits.ta.poke(false.B)
      state.bits.xrm.poke(0.U)
      state.bits.sew.poke(0.U)
      state.bits.lmul.poke(0.U)
      state.bits.lmul_orig.poke(0.U)
      state.bits.vill.poke(false.B)
    }
  }
}

class PmuCsrDecodeSpec extends AnyFreeSpec with ChiselSim {
  for (xlen <- Seq(32, 64)) {
    s"RV$xlen PMU CSR permissions" in {
      val p = new Parameters(xlen = xlen)
      simulate(new DispatchV2(p)) { dut =>
        // Make lane 0 eligible for CSR dispatch
        DispatchTestUtils.initialize(dut, p)
        dut.io.inst(0).valid.poke(true.B)

        def check(address: BigInt, funct3: Int, source: Int, supported: Boolean): Unit = {
          val instruction =
            (address << 20) | (source << 15) | (funct3 << 12) | (1 << 7) | 0x73
          dut.io.inst(0).bits.inst.poke(instruction.U)
          dut.io.csr.valid.expect(supported.B)
          dut.io.csrFault(0).expect((!supported).B)
        }

        // Check range boundaries, alias permissions and XLEN-dependent high halves
        val registers = Seq(
          (CsrAddress.MCYCLE, false, false),
          (CsrAddress.MINSTRET, false, false),
          (CsrAddress.CYCLE, true, false),
          (CsrAddress.INSTRET, true, false),
          (CsrAddress.MCYCLEH, false, true),
          (CsrAddress.MINSTRETH, false, true),
          (CsrAddress.CYCLEH, true, true),
          (CsrAddress.INSTRETH, true, true),
          (CsrAddress.MCOUNTINHIBIT, false, false),
          (CsrAddress.MHPMEVENT3, false, false),
          (CsrAddress.MHPMEVENT31, false, false),
          (CsrAddress.MHPMCOUNTER3, false, false),
          (CsrAddress.MHPMCOUNTER31, false, false),
          (CsrAddress.HPMCOUNTER3, true, false),
          (CsrAddress.HPMCOUNTER31, true, false),
          (CsrAddress.MHPMCOUNTER3H, false, true),
          (CsrAddress.MHPMCOUNTER31H, false, true),
          (CsrAddress.HPMCOUNTER3H, true, true),
          (CsrAddress.HPMCOUNTER31H, true, true)
        )
        for {
          (address, read_only, high) <- registers
          funct3                     <- Seq(1, 2)
        } {
          val supported = (!high || xlen == 32) && (!read_only || funct3 == 2)
          check(address.litValue, funct3, 0, supported)
        }

        // Source-zero suppression depends on instruction form, not CSR address
        for {
          funct3 <- Seq(1, 2, 3, 5, 6, 7)
          source <- Seq(0, 5)
        } {
          val writes = funct3 == 1 || funct3 == 5 || source != 0
          check(CsrAddress.CYCLE.litValue, funct3, source, !writes)
        }

        // Reserved time slots and holes between PMU register banks remain illegal
        for (address <- Seq(0xb01, 0xb81, 0xc01, 0xc81, 0xb20, 0xb7f, 0xc7f, 0x321, 0x322)) {
          check(BigInt(address), 2, 0, false)
        }
      }
    }
  }
}

class DispatchLsuStallSpec extends AnyFreeSpec with ChiselSim {
  val p = new Parameters()

  private def load(rd: Int): BigInt = (10 << 15) | (2 << 12) | (rd << 7) | 0x03
  private def add(rd: Int): BigInt  = (rd << 7) | 0x13

  private def instruction(dut: DispatchV2, lane: Int, word: BigInt): Unit = {
    dut.io.inst(lane).valid.poke(true.B)
    dut.io.inst(lane).bits.inst.poke(word.U)
  }

  "LSU capacity stall attribution" in {
    simulate(new DispatchV2(p)) { dut =>
      // Observe a blocked load before request valid is suppressed
      DispatchTestUtils.initialize(dut, p)
      instruction(dut, 0, load(1))
      dut.io.lsuQueueCapacity.poke(0.U)
      dut.io.lsuStall.expect(true.B)
      dut.io.lsu(0).valid.expect(false.B)
      dut.io.inst(0).ready.expect(false.B)

      dut.io.lsuQueueCapacity.poke(1.U)
      dut.io.lsuStall.expect(false.B)
      dut.io.lsu(0).valid.expect(true.B)
      dut.io.inst(0).ready.expect(true.B)

      // Empty and non-memory instructions do not stall on LSU capacity
      dut.io.lsuQueueCapacity.poke(0.U)
      dut.io.inst(0).valid.poke(false.B)
      dut.io.lsuStall.expect(false.B)
      instruction(dut, 0, add(1))
      dut.io.lsuStall.expect(false.B)
      dut.io.inst(0).ready.expect(true.B)

      // Exclude cycles already blocked by other dispatch conditions
      val blockers: Seq[DispatchV2 => Unit] = Seq(
        _.io.interlock.poke(true.B),
        _.io.scoreboard.regd.poke((1 << 10).U),
        _.io.retirement_buffer_nSpace.poke(0.U)
      )
      for (block <- blockers) {
        DispatchTestUtils.initialize(dut, p)
        instruction(dut, 0, load(1))
        dut.io.lsuQueueCapacity.poke(0.U)
        dut.io.lsuStall.expect(true.B)
        block(dut)
        dut.io.lsuStall.expect(false.B)
        dut.io.inst(0).ready.expect(false.B)
      }

      // Earlier dispatch lanes consume capacity or block younger instructions
      DispatchTestUtils.initialize(dut, p)
      instruction(dut, 0, load(1))
      instruction(dut, 1, add(2))
      instruction(dut, 2, load(3))
      dut.io.lsuQueueCapacity.poke(1.U)
      dut.io.inst(0).ready.expect(true.B)
      dut.io.inst(1).ready.expect(true.B)
      dut.io.inst(2).ready.expect(false.B)
      dut.io.lsuStall.expect(true.B)

      // Enough entries allow all lanes to dispatch
      dut.io.lsuQueueCapacity.poke(2.U)
      dut.io.inst(2).ready.expect(true.B)
      dut.io.lsuStall.expect(false.B)

      // A missing or blocked older instruction must not report a younger LSU stall
      dut.io.lsuQueueCapacity.poke(1.U)
      dut.io.inst(0).valid.poke(false.B)
      dut.io.lsuStall.expect(false.B)

      instruction(dut, 0, load(1))
      dut.io.scoreboard.comb.poke((1 << 1).U)
      dut.io.lsuStall.expect(false.B)

      // Backpressure on an older non-LSU unit takes precedence
      DispatchTestUtils.initialize(dut, p)
      instruction(dut, 0, BigInt("02b500b3", 16)) // mul x1, x10, x11
      instruction(dut, 1, load(2))
      instruction(dut, 2, load(3))
      dut.io.lsuQueueCapacity.poke(1.U)
      dut.io.mlu(0).ready.poke(false.B)
      dut.io.lsuStall.expect(false.B)
      dut.io.mlu(0).ready.poke(true.B)
      dut.io.inst(1).ready.expect(true.B)
      dut.io.inst(2).ready.expect(false.B)
      dut.io.lsuStall.expect(true.B)
    }
  }

  "LSU stalls exclude vector configuration and vector queue stalls" in {
    val vector_parameters = new Parameters()
    vector_parameters.enableRvv = true
    simulate(new DispatchV2(vector_parameters)) { dut =>
      DispatchTestUtils.initialize(dut, vector_parameters)
      instruction(dut, 0, BigInt("02050087", 16)) // vle8.v v1, (x10)
      dut.io.lsuQueueCapacity.poke(0.U)
      dut.io.lsuStall.expect(true.B)

      // Vector state and queue capacity are separate blockers
      dut.io.rvvState.get.valid.poke(false.B)
      dut.io.lsuStall.expect(false.B)
      dut.io.rvvState.get.valid.poke(true.B)
      dut.io.rvvState.get.bits.vill.poke(true.B)
      dut.io.lsuStall.expect(false.B)
      dut.io.rvvState.get.bits.vill.poke(false.B)
      dut.io.rvvQueueCapacity.get.poke(0.U)
      dut.io.lsuStall.expect(false.B)
      dut.io.rvvQueueCapacity.get.poke(1.U)
      dut.io.lsuStall.expect(true.B)
    }
  }
}
