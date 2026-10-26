package coralnpu

import chisel3._

object CsrAddress extends ChiselEnum {
  // Per spec, this is not allocated. We use this internally to
  // represent an invalid address.
  val RESERVED       = Value(0x000.U(12.W))
  val FFLAGS         = Value(0x001.U(12.W))
  val FRM            = Value(0x002.U(12.W))
  val FCSR           = Value(0x003.U(12.W))
  val VSTART         = Value(0x008.U(12.W))
  val VXSAT          = Value(0x009.U(12.W))
  val VXRM           = Value(0x00a.U(12.W))
  val MSTATUS        = Value(0x300.U(12.W))
  val MISA           = Value(0x301.U(12.W))
  val MIE            = Value(0x304.U(12.W))
  val MTVEC          = Value(0x305.U(12.W))
  val MSTATUSH       = Value(0x310.U(12.W))
  val MCOUNTINHIBIT  = Value(0x320.U(12.W))
  val MHPMEVENT3     = Value(0x323.U(12.W))
  val MHPMEVENT4     = Value(0x324.U(12.W))
  val MHPMEVENT5     = Value(0x325.U(12.W))
  val MHPMEVENT6     = Value(0x326.U(12.W))
  val MHPMEVENT31    = Value(0x33f.U(12.W))
  val MSCRATCH       = Value(0x340.U(12.W))
  val MEPC           = Value(0x341.U(12.W))
  val MCAUSE         = Value(0x342.U(12.W))
  val MTVAL          = Value(0x343.U(12.W))
  val MIP            = Value(0x344.U(12.W))
  val TSELECT        = Value(0x7a0.U(12.W))
  val TDATA1         = Value(0x7a1.U(12.W))
  val TDATA2         = Value(0x7a2.U(12.W))
  val TINFO          = Value(0x7a4.U(12.W))
  val DCSR           = Value(0x7b0.U(12.W))
  val DPC            = Value(0x7b1.U(12.W))
  val DSCRATCH0      = Value(0x7b2.U(12.W))
  val DSCRATCH1      = Value(0x7b3.U(12.W))
  val MCONTEXT0      = Value(0x7c0.U(12.W))
  val MCONTEXT1      = Value(0x7c1.U(12.W))
  val MCONTEXT2      = Value(0x7c2.U(12.W))
  val MCONTEXT3      = Value(0x7c3.U(12.W))
  val MCONTEXT4      = Value(0x7c4.U(12.W))
  val MCONTEXT5      = Value(0x7c5.U(12.W))
  val MCONTEXT6      = Value(0x7c6.U(12.W))
  val MCONTEXT7      = Value(0x7c7.U(12.W))
  val MPC            = Value(0x7e0.U(12.W))
  val MSP            = Value(0x7e1.U(12.W))
  val MCYCLE         = Value(0xb00.U(12.W))
  val MINSTRET       = Value(0xb02.U(12.W))
  val MHPMCOUNTER3   = Value(0xb03.U(12.W))
  val MHPMCOUNTER4   = Value(0xb04.U(12.W))
  val MHPMCOUNTER5   = Value(0xb05.U(12.W))
  val MHPMCOUNTER6   = Value(0xb06.U(12.W))
  val MHPMCOUNTER31  = Value(0xb1f.U(12.W))
  val MCYCLEH        = Value(0xb80.U(12.W))
  val MINSTRETH      = Value(0xb82.U(12.W))
  val MHPMCOUNTER3H  = Value(0xb83.U(12.W))
  val MHPMCOUNTER4H  = Value(0xb84.U(12.W))
  val MHPMCOUNTER5H  = Value(0xb85.U(12.W))
  val MHPMCOUNTER6H  = Value(0xb86.U(12.W))
  val MHPMCOUNTER31H = Value(0xb9f.U(12.W))
  val CYCLE          = Value(0xc00.U(12.W))
  val INSTRET        = Value(0xc02.U(12.W))
  val HPMCOUNTER3    = Value(0xc03.U(12.W))
  val HPMCOUNTER31   = Value(0xc1f.U(12.W))
  val VL             = Value(0xc20.U(12.W))
  val VTYPE          = Value(0xc21.U(12.W))
  val VLENB          = Value(0xc22.U(12.W))
  val MTYPE          = Value(0xc23.U(12.W))
  val CYCLEH         = Value(0xc80.U(12.W))
  val INSTRETH       = Value(0xc82.U(12.W))
  val HPMCOUNTER3H   = Value(0xc83.U(12.W))
  val HPMCOUNTER31H  = Value(0xc9f.U(12.W))
  val MVENDORID      = Value(0xf11.U(12.W))
  val MARCHID        = Value(0xf12.U(12.W))
  val MIMPID         = Value(0xf13.U(12.W))
  val MHARTID        = Value(0xf14.U(12.W))
  val KISA           = Value(0xfc0.U(12.W))
  val KSCM0          = Value(0xfc4.U(12.W))
  val KSCM1          = Value(0xfc8.U(12.W))
  val KSCM2          = Value(0xfcc.U(12.W))
  val KSCM3          = Value(0xfd0.U(12.W))
  val KSCM4          = Value(0xfd4.U(12.W))

  def isReadOnly(index: UInt): Bool = index(11, 10) === 3.U

  def isSupported(index: UInt, xlen: Int): Bool = {
    val (_, known) = safe(index)
    val pmu        = PmuCsrAddress.decode(index, xlen)
    Mux(pmu.matches, pmu.supported, known)
  }

  def canAccess(index: UInt, xlen: Int, write: Bool): Bool =
    isSupported(index, xlen) && !(write && isReadOnly(index))
}

object PmuCsrAddress {
  val cycleSlot               = 0
  val instretSlot             = 2
  val firstHpmSlot            = 3
  val lastCounterSlot         = 31
  val maxProgrammableCounters = lastCounterSlot - firstHpmSlot + 1

  // Aliases and RV32 halves share an architectural slot, independent of physical storage
  case class Decoded(
    counter: Bool,
    selector: Bool,
    inhibit: Bool,
    slot: UInt,
    high: Bool,
    readOnly: Bool,
    supported: Bool
  ) {
    def matches: Bool  = counter || selector || inhibit
    def writable: Bool = supported && !readOnly
  }

  def decode(index: UInt, xlen: Int): Decoded = {
    def inRange(first: CsrAddress.Type, last: CsrAddress.Type): Bool =
      index >= first.asUInt && index <= last.asUInt

    val low = inRange(CsrAddress.MCYCLE, CsrAddress.MHPMCOUNTER31) ||
      inRange(CsrAddress.CYCLE, CsrAddress.HPMCOUNTER31)
    val high = inRange(CsrAddress.MCYCLEH, CsrAddress.MHPMCOUNTER31H) ||
      inRange(CsrAddress.CYCLEH, CsrAddress.HPMCOUNTER31H)
    val slot = index(4, 0)
    // Slot 1 is time (not implemented in hart)
    val counter   = (low || high) && slot =/= 1.U
    val selector  = inRange(CsrAddress.MHPMEVENT3, CsrAddress.MHPMEVENT31)
    val inhibit   = index === CsrAddress.MCOUNTINHIBIT.asUInt
    val supported = (counter || selector || inhibit) &&
      (if (xlen == 32) true.B else !high)
    Decoded(
      counter,
      selector,
      inhibit,
      slot,
      high,
      CsrAddress.isReadOnly(index),
      supported
    )
  }
}
