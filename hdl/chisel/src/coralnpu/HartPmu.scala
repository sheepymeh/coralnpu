package coralnpu

import chisel3._
import chisel3.util._
import common.{BitWidthForInt, MuxUpTo1H}

class HartPmuEvents(p: Parameters) extends Bundle {
  val nRetired = UInt(BitWidthForInt(p.retirementLanes))
}

class PmuCsrRequest(p: Parameters) extends Bundle {
  val valid   = Bool()
  val address = UInt(12.W)
  val op      = CsrOp()
  val operand = UInt(p.xlen.W)
  val write   = Bool() // Caller suppresses CSRRS/CSRRC with rs1=x0
}

class PmuCsrResponse(p: Parameters) extends Bundle {
  val readData  = UInt(p.xlen.W)
  val writeData = UInt(p.xlen.W)
}

class PmuCounterStatus extends Bundle {
  val cycle   = UInt(64.W)
  val instret = UInt(64.W)
}

/** Execute-stage access, viewed from Csr, with no handshake or additional pipeline stage
  *
  * Request carries registered command and live execute-stage operand. Read data is combinational,
  * including forwarded retirement. Write data is the effective XLEN word after RMW/legalization,
  * zero unless a valid writable request writes. Writes commit at closing edge, with reset taking
  * priority. Status exposes stored counts without forwarding
  */
class PmuCsrIO(p: Parameters) extends Bundle {
  val request  = Output(new PmuCsrRequest(p))
  val response = Input(new PmuCsrResponse(p))
  val status   = Input(new PmuCounterStatus)
}

// Events and counters share hart clock/reset
class HartPmu(p: Parameters) extends Module {
  require(
    p.numProgrammableHpmCounters >= 0 &&
      p.numProgrammableHpmCounters <= PmuCsrAddress.maxProgrammableCounters,
    s"numProgrammableHpmCounters must be between 0 and ${PmuCsrAddress.maxProgrammableCounters}"
  )

  val io = IO(new Bundle {
    val csr    = Flipped(new PmuCsrIO(p))
    val events = Input(new HartPmuEvents(p))
  })

  val request = io.csr.request
  val address = PmuCsrAddress.decode(request.address, p.xlen)
  val write   = request.valid && request.write && address.writable

  // Append subsystem event tables here, IDs drive both validation and increment selection
  val event_increments = Seq(0 -> 0.U)
  val event_ids        = event_increments.map(_._1)
  require(event_ids.distinct.size == event_ids.size, "PMU event IDs must be unique")
  require(event_ids.forall(_ >= 0), "PMU event IDs must be nonnegative")
  val event_width = math.max(1, BigInt(event_ids.max).bitLength)

  val hpm_slots = PmuCsrAddress.firstHpmSlot until
    (PmuCsrAddress.firstHpmSlot + p.numProgrammableHpmCounters)
  val counter_slots     = Seq(PmuCsrAddress.cycleSlot, PmuCsrAddress.instretSlot) ++ hpm_slots
  val counters          = Seq.fill(counter_slots.size)(RegInit(0.U(64.W)))
  val selectors         = Seq.fill(hpm_slots.size)(RegInit(0.U(event_width.W))) // event selector
  val mcycle            = counters(0)
  val minstret          = counters(1)
  val mcountinhibit     = RegInit(0.U(32.W))
  val countinhibit_mask =
    counter_slots.foldLeft(BigInt(0))((mask, slot) => mask.setBit(slot)).U(32.W)

  val counter_select        = counter_slots.map(slot => address.counter && address.slot === slot.U)
  val any_counter_selected  = counter_select.reduce(_ || _)
  val selector_select       = hpm_slots.map(slot => address.selector && address.slot === slot.U)
  val any_selector_selected = selector_select.foldLeft(false.B)(_ || _)

  val minstret_written = write && address.counter && address.slot === PmuCsrAddress.instretSlot.U
  // Retirement arrives one cycle after CSR commit, including inhibit-writing instruction
  val minstret_written_delayed = RegNext(minstret_written, false.B)
  val minstret_inhibit_delayed = RegNext(mcountinhibit(PmuCsrAddress.instretSlot), false.B)
  val nRetired = Mux(minstret_written_delayed || minstret_inhibit_delayed, 0.U, io.events.nRetired)

  private def readWord(value: UInt): UInt =
    if (p.xlen == 32) Mux(address.high, value(63, 32), value(31, 0)) else value

  private def replaceWord(value: UInt, data: UInt): UInt =
    if (p.xlen == 32)
      Mux(address.high, Cat(data, value(31, 0)), Cat(value(63, 32), data))
    else data

  val counter_values = counter_slots.zip(counters).map { case (slot, value) =>
    if (slot == PmuCsrAddress.instretSlot) value + nRetired else value
  }
  val read_data = MuxUpTo1H(
    0.U(p.xlen.W),
    counter_select.zip(counter_values.map(readWord)) ++
      selector_select.zip(selectors.map(_.pad(p.xlen))) ++
      Seq(address.inhibit -> mcountinhibit.pad(p.xlen))
  )
  io.csr.response.readData := Mux(address.supported, read_data, 0.U)

  val raw_write_data = CsrOp.writeData(request.op, read_data, request.operand)
  // Unsupported IDs disable selection instead of truncating to a supported ID
  val selector_write_data = MuxLookup(raw_write_data, 0.U(event_width.W))(
    event_ids.map(id => id.U -> id.U(event_width.W))
  )
  val write_data = MuxUpTo1H(
    0.U(p.xlen.W),
    Seq(
      any_counter_selected  -> raw_write_data,
      any_selector_selected -> selector_write_data.pad(p.xlen),
      address.inhibit       -> (raw_write_data & countinhibit_mask.pad(p.xlen))
    )
  )
  io.csr.response.writeData := Mux(write, write_data, 0.U)

  when(write && address.inhibit) { mcountinhibit := write_data(31, 0) }
  for (i <- selectors.indices) {
    when(write && selector_select(i)) { selectors(i) := selector_write_data }
  }
  for ((slot, i) <- counter_slots.zipWithIndex) {
    val increment = slot match {
      case PmuCsrAddress.cycleSlot =>
        Mux(mcountinhibit(slot), 0.U, 1.U)
      case PmuCsrAddress.instretSlot =>
        nRetired
      case _ =>
        val selected_increment = MuxLookup(
          selectors(slot - PmuCsrAddress.firstHpmSlot),
          0.U
        )(
          event_increments.map { case (id, amount) =>
            id.U -> amount
          }
        )
        Mux(mcountinhibit(slot), 0.U, selected_increment)
    }

    // RV32 writes preserve untouched half, including retirement from older instructions
    counters(i) := Mux(
      write && counter_select(i),
      replaceWord(counter_values(i), write_data),
      counters(i) + increment
    )
  }

  io.csr.status.cycle   := mcycle
  io.csr.status.instret := minstret
}
