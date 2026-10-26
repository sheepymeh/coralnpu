// Copyright 2023 Google LLC
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

import common.{MakeInvalid, MakeValid, MuxUpTo1H}
import chisel3._
import chisel3.util._
import coralnpu.float.CsrFloatIO

class CsrRvvIO(p: Parameters) extends Bundle {
  // To Csr from RvvCore
  val vstart = Input(UInt(log2Ceil(p.rvvVlen).W))
  val vl     = Input(UInt(log2Ceil(p.rvvVlen).W))
  val vtype  = Input(UInt(p.xlen.W))
  val vxrm   = Input(UInt(2.W))
  val vxsat  = Input(Bool())
  // VME (Zvt). Tied to 0 when enableVme=false.
  val mtype  = Input(UInt(p.xlen.W))
  val fflags = Input(Valid(UInt(5.W)))
  // From Csr to RvvCore
  val vstart_write = Output(Valid(UInt(log2Ceil(p.rvvVlen).W)))
  val vxrm_write   = Output(Valid(UInt(2.W)))
  val vxsat_write  = Output(Valid(Bool()))
  val frm          = Output(UInt(3.W))
}

object Csr {
  def apply(p: Parameters): Csr = {
    return Module(new Csr(p))
  }
}

object CsrMode extends ChiselEnum {
  val Machine = Value(0.U(2.W))
  val Debug   = Value(2.U(2.W))
}

/* For details, see The RISC-V Debug Specification v1.0, chapter 4.9.1 */
class Dcsr(p: Parameters) extends Bundle {
  val debugver  = UInt(4.W)
  val extcause  = UInt(3.W)
  val cetrig    = Bool()
  val pelp      = Bool()
  val ebreakvs  = Bool()
  val ebreakvu  = Bool()
  val ebreakm   = Bool()
  val ebreaks   = Bool()
  val ebreaku   = Bool()
  val stepie    = Bool()
  val stopcount = Bool()
  val stoptime  = Bool()
  val cause     = UInt(3.W)
  val v         = Bool()
  val mprven    = Bool()
  val nmip      = Bool()
  val step      = Bool()
  val prv       = UInt(2.W)

  def asWord: UInt = {
    val ret = Cat(
      debugver,
      0.U(1.W),
      extcause,
      0.U(4.W),
      cetrig,
      pelp,
      ebreakvs,
      ebreakvu,
      ebreakm,
      0.U(1.W),
      ebreaks,
      ebreaku,
      stepie,
      stopcount,
      stoptime,
      cause,
      v,
      mprven,
      nmip,
      step,
      prv
    )
    assert(ret.getWidth == 32)
    if (p.xlen == 32) {
      ret
    } else {
      Cat(0.U((p.xlen - 32).W), ret)
    }
  }
}

/* For details, see The RISC-V Debug Specification v1.0, chapter 5.7.2 */
class Tdata1(p: Parameters) extends Bundle {
  val data         = UInt(p.xlen.W)
  def _type: UInt  = data(p.xlen - 1, p.xlen - 4)
  def asWord: UInt = {
    data.asUInt
  }
  def isTrigger6: Bool = {
    _type === 6.U(4.W)
  }
  def m: Bool = data(6)
}

// Cause types for the dcsr `cause` field.
// See Table 8 in Chapter 4.9.1 of the Debug Specification.
// These are sorted in priority order.
object DebugCause extends ChiselEnum {
  val resethaltreq = 5.U(3.W)
  val haltgroup    = 6.U(3.W)
  val haltreq      = 3.U(3.W)
  val trigger      = 2.U(3.W)
  val ebreak       = 1.U(3.W)
  val step         = 4.U(3.W)
  val other        = 7.U(3.W)
}

class CsrBruIO(p: Parameters) extends Bundle {
  val in = new Bundle {
    val mode   = Valid(CsrMode())
    val mcause = Valid(UInt(p.xlen.W))
    val mepc   = Valid(UInt(p.programCounterBits.W))
    val mtval  = Valid(UInt(p.xlen.W))
    val halt   = Output(Bool())
    val fault  = Output(Bool())
    val wfi    = Output(Bool())
  }
  val out = new Bundle {
    val mode            = Input(CsrMode())
    val mepc            = Input(UInt(p.programCounterBits.W))
    val mtvec           = Input(UInt(p.programCounterBits.W))
    val interrupt       = Input(Bool())
    val interrupt_cause = Input(UInt(p.xlen.W))
  }
  def defaults() = {
    out.mode            := CsrMode.Machine
    out.mepc            := 0.U
    out.mtvec           := 0.U
    out.interrupt       := false.B
    out.interrupt_cause := 0.U
  }
}

class Csr(p: Parameters) extends Module {
  val pmu = IO(new PmuCsrIO(p))
  val io  = IO(new Bundle {
    // Reset and shutdown.
    val csr = new CsrInOutIO(p)

    // Decode cycle.
    val req = Flipped(Valid(new CsrCmd(p)))

    // Execute cycle.
    val rs1          = Flipped(new RegfileReadDataIO(p))
    val rd           = Valid(Flipped(new RegfileWriteDataIO(p)))
    val bru          = Flipped(new CsrBruIO(p))
    val float        = Option.when(p.enableFloat) { Flipped(new CsrFloatIO(p)) }
    val rvv          = Option.when(p.enableRvv) { new CsrRvvIO(p) }
    val float_dirty  = Option.when(p.enableFloat)(Input(Bool()))
    val rvv_dirty    = Option.when(p.enableRvv)(Input(Bool()))
    val vme_discard  = Option.when(p.enableVme)(Input(Bool()))
    val vme_dirty    = Option.when(p.enableVme)(Input(Bool()))
    val vme_fp_dirty = Option.when(p.enableVme && p.enableFloat)(Input(Bool()))
    val mstatus_ms   = Option.when(p.enableVme)(Output(UInt(2.W)))

    // Pipeline Control.
    val halted = Output(Bool())
    val fault  = Output(Bool())
    val wfi    = Output(Bool())
    val irq    = Input(Bool())
    val dm     = new Bundle {
      val debug_req           = Input(Bool())
      val resume_req          = Input(Bool())
      val debug_mode          = Output(Bool())
      val entering_debug_mode = Output(Bool())
      val single_step         = Output(Bool())
      val dcsr_step           = Output(Bool())
      val current_pc          = Input(UInt(p.programCounterBits.W))
      val next_pc             = Input(UInt(p.programCounterBits.W))
      val debug_pc            = Valid(UInt(p.fetchAddrBits.W))
    }
    val timer_irq    = Input(Bool())
    val software_irq = Input(Bool())
    val trace        = Output(new CsrTraceIO(p))
  })

  def LegalizeTdata1(wdata: UInt): Tdata1 = {
    assert(wdata.getWidth == p.xlen)
    val newWdata = Wire(new Tdata1(p))
    newWdata.data := Cat(
      6.U(4.W),          // type
      wdata(p.xlen - 5), // dmode
      0.U((p.xlen - 21).W),
      wdata(15, 12) & 1.U(4.W), // action
      0.U(5.W),
      wdata(6),                // m
      (wdata(5, 0) & 4.U(6.W)) // !uncertainen, !s, !u, execute, !store, !load
    )
    newWdata
  }

  // Control registers. CsrAddress.RESERVED is used for invalid values.
  val req = RegInit(MakeInvalid(new CsrCmd(p)))
  req := MakeValid(io.req.valid, io.req.bits, bitsWhenInvalid = req.bits)

  // Pipeline Control.
  val halted = RegInit(false.B)
  val fault  = RegInit(false.B)
  val wfi    = RegInit(false.B)

  // Machine(0)/Debug(2) Mode.
  val mode = RegInit(CsrMode.Machine)

  // CSRs parallel loaded when(reset).
  val mpc       = RegInit(0.U(p.programCounterBits.W))
  val msp       = RegInit(0.U(p.xlen.W))
  val mcause    = RegInit(0.U(p.xlen.W))
  val mtval     = RegInit(0.U(p.xlen.W))
  val mcontext0 = RegInit(0.U(p.xlen.W))
  val mcontext1 = RegInit(0.U(p.xlen.W))
  val mcontext2 = RegInit(0.U(p.xlen.W))
  val mcontext3 = RegInit(0.U(p.xlen.W))
  val mcontext4 = RegInit(0.U(p.xlen.W))
  val mcontext5 = RegInit(0.U(p.xlen.W))
  val mcontext6 = RegInit(0.U(p.xlen.W))
  val mcontext7 = RegInit(0.U(p.xlen.W))

  // Debug mode CSRs
  val dcsr      = RegInit(0.U.asTypeOf(new Dcsr(p)))
  val dpc       = RegInit(0.U(p.programCounterBits.W))
  val dscratch0 = RegInit(0.U(p.xlen.W))
  val dscratch1 = RegInit(0.U(p.xlen.W))
  // Trigger CSRs
  val tselect = RegInit(0.U(p.xlen.W))
  val tdata1  = RegInit(
    ((if (p.xlen == 32) 0x60000000L else 0x6000000000000000L).U).asTypeOf(new Tdata1(p))
  )
  val tdata2 = RegInit(0.U(p.xlen.W))
  /* For details, see The RISC-V Debug Specification v1.0, chapter 5.7.5 */
  val tinfo = RegInit(0x01000040.U(p.xlen.W))

  // CSRs with initialization.
  val fflags       = RegInit(0.U(5.W))
  val frm          = RegInit(0.U(3.W))
  val mstatus_mie  = RegInit(false.B)
  val mstatus_mpie = RegInit(false.B)
  val mie          = RegInit(0.U(p.xlen.W))
  val mtvec        = RegInit(0.U(p.programCounterBits.W))
  val mscratch     = RegInit(0.U(p.xlen.W))
  val mepc         = RegInit(0.U(p.programCounterBits.W))
  val mhartid      = RegInit(p.hartId.U(p.xlen.W))

  // MXLEN, I,M,X extensions
  val misaValue = (if (p.xlen == 32) BigInt(0x40801100L) else BigInt("8000000000801100", 16)) |
    (if (p.enableFloat) { BigInt(1) << 5 }
     else { BigInt(0) })
  val misa = RegInit(misaValue.U(p.xlen.W))
  // CoralNPU-specific ISA register.
  val kisa = RegInit(0.U(p.xlen.W))
  // SCM Revision (spread over 5 indices)
  val kscm = RegInit(((new ScmInfo).revision).U(160.W))

  // 0x426 - Google's Vendor ID
  val mvendorid = RegInit(0x426.U(p.xlen.W))

  // Unimplemented -- explicitly return zero.
  val marchid = RegInit(0.U(1.W))
  val mimpid  = RegInit(0.U(1.W))

  val fcsr = Cat(frm, fflags)

  val fs = Option.when(p.enableFloat)(RegInit(1.U(2.W)))
  val vs = Option.when(p.enableRvv)(RegInit(1.U(2.W)))
  val ms = Option.when(p.enableVme)(RegInit(1.U(2.W)))

  val fs_val = fs.getOrElse(0.U(2.W))
  val vs_val = vs.getOrElse(0.U(2.W))
  val ms_val = ms.getOrElse(0.U(2.W))
  if (p.enableVme) {
    io.mstatus_ms.get := ms_val
  }
  val mstatus_sd = (fs_val === 3.U) || (vs_val === 3.U) || (ms_val === 3.U)

  val pmuEn = PmuCsrAddress.decode(req.bits.index, p.xlen).supported

  // Decode the Index.
  val (csr_address, _) = CsrAddress.safe(req.bits.index)
  assert(!(req.valid && !CsrAddress.isSupported(req.bits.index, p.xlen)))
  val fflagsEn   = csr_address === CsrAddress.FFLAGS
  val frmEn      = csr_address === CsrAddress.FRM
  val fcsrEn     = csr_address === CsrAddress.FCSR
  val vstartEn   = Option.when(p.enableRvv) { csr_address === CsrAddress.VSTART }
  val vlEn       = Option.when(p.enableRvv) { csr_address === CsrAddress.VL }
  val vtypeEn    = Option.when(p.enableRvv) { csr_address === CsrAddress.VTYPE }
  val vxrmEn     = Option.when(p.enableRvv) { csr_address === CsrAddress.VXRM }
  val vxsatEn    = Option.when(p.enableRvv) { csr_address === CsrAddress.VXSAT }
  val mstatusEn  = csr_address === CsrAddress.MSTATUS
  val misaEn     = csr_address === CsrAddress.MISA
  val mieEn      = csr_address === CsrAddress.MIE
  val mtvecEn    = csr_address === CsrAddress.MTVEC
  val mstatushEn = csr_address === CsrAddress.MSTATUSH
  val mscratchEn = csr_address === CsrAddress.MSCRATCH
  val mepcEn     = csr_address === CsrAddress.MEPC
  val mcauseEn   = csr_address === CsrAddress.MCAUSE
  val mtvalEn    = csr_address === CsrAddress.MTVAL
  val mipEn      = csr_address === CsrAddress.MIP
  // Debug CSRs.
  val tselectEn   = csr_address === CsrAddress.TSELECT
  val tdata1En    = csr_address === CsrAddress.TDATA1
  val tdata2En    = csr_address === CsrAddress.TDATA2
  val tinfoEn     = csr_address === CsrAddress.TINFO
  val dcsrEn      = csr_address === CsrAddress.DCSR
  val dpcEn       = csr_address === CsrAddress.DPC
  val dscratch0En = csr_address === CsrAddress.DSCRATCH0
  val dscratch1En = csr_address === CsrAddress.DSCRATCH1
  val mcontext0En = csr_address === CsrAddress.MCONTEXT0
  val mcontext1En = csr_address === CsrAddress.MCONTEXT1
  val mcontext2En = csr_address === CsrAddress.MCONTEXT2
  val mcontext3En = csr_address === CsrAddress.MCONTEXT3
  val mcontext4En = csr_address === CsrAddress.MCONTEXT4
  val mcontext5En = csr_address === CsrAddress.MCONTEXT5
  val mcontext6En = csr_address === CsrAddress.MCONTEXT6
  val mcontext7En = csr_address === CsrAddress.MCONTEXT7
  val mpcEn       = csr_address === CsrAddress.MPC
  val mspEn       = csr_address === CsrAddress.MSP
  // Vector CSRs.
  val vlenbEn = Option.when(p.enableRvv) { csr_address === CsrAddress.VLENB }
  // Matrix CSRs (Zvt). Read-only; updated by mset* through the RVV frontend.
  val mtypeEn = Option.when(p.enableVme) { csr_address === CsrAddress.MTYPE }

  // M-mode information CSRs.
  val mvendoridEn = csr_address === CsrAddress.MVENDORID
  val marchidEn   = csr_address === CsrAddress.MARCHID
  val mimpidEn    = csr_address === CsrAddress.MIMPID
  val mhartidEn   = csr_address === CsrAddress.MHARTID
  // Start of custom CSRs.
  val kisaEn  = csr_address === CsrAddress.KISA
  val kscm0En = csr_address === CsrAddress.KSCM0
  val kscm1En = csr_address === CsrAddress.KSCM1
  val kscm2En = csr_address === CsrAddress.KSCM2
  val kscm3En = csr_address === CsrAddress.KSCM3
  val kscm4En = csr_address === CsrAddress.KSCM4

  // Pipeline Control.
  when(io.bru.in.halt) {
    halted := true.B
  }

  when(io.bru.in.fault) {
    fault := true.B
  }

  val mtip_pending = io.timer_irq && mie(7)
  val meip_pending = io.irq && mie(11)
  val msip_pending = io.software_irq && mie(3)
  wfi := Mux(wfi, !(mtip_pending || meip_pending || msip_pending || io.dm.debug_req), io.bru.in.wfi)

  io.halted := halted
  io.fault  := fault
  io.wfi    := wfi

  assert(!(io.fault && !io.halted && !io.wfi))

  // Register state.
  val rs1 = io.rs1.data

  // Helper to compute local write data to avoid global rdata/wdata timing paths
  private def localWdata(reg_val: UInt): UInt = CsrOp.writeData(req.bits.op, reg_val, rs1)

  // WARL legalization for FS and VS: Coral NPU does not support disabling the
  // extension units (00 -> 11 Dirty).
  private def legalizeFsVs(v: UInt): UInt = Mux(v === 0.U, 3.U(2.W), v)

  // Common bitfield formatting helpers shared by rdata and trace_data
  private def mstatusWord(
    mie: Bool,
    mpie: Bool,
    fs_val: UInt,
    vs_val: UInt,
    ms_val: UInt,
    sd_val: Bool
  ): UInt = Cat(
    sd_val,
    ms_val,
    0.U((p.xlen - 18).W),
    fs_val,
    3.U(2.W),
    vs_val,
    0.U(1.W),
    mpie,
    0.U(3.W),
    mie,
    0.U(3.W)
  )

  private def fflagsWord(f: UInt): UInt = Cat(0.U((p.xlen - 5).W), f(4, 0))
  private def frmWord(m: UInt): UInt    = Cat(0.U((p.xlen - 3).W), m(2, 0))
  private def fcsrWord(c: UInt): UInt   = Cat(0.U((p.xlen - 8).W), c(7, 0))
  private def vstartWord(v: UInt): UInt =
    Cat(0.U((p.xlen - log2Ceil(p.rvvVlen)).W), v(log2Ceil(p.rvvVlen) - 1, 0))
  private def vxrmWord(v: UInt): UInt  = Cat(0.U((p.xlen - 2).W), v(1, 0))
  private def vxsatWord(v: UInt): UInt = Cat(0.U((p.xlen - 1).W), v(0))
  private def mepcWord(v: UInt): UInt  =
    Cat(v(p.programCounterBits - 1, 2), 0.U(2.W))

  val mip = Cat(
    0.U((p.xlen - 12).W),
    io.irq,
    0.U(3.W),
    io.timer_irq,
    0.U(3.W),
    io.software_irq,
    0.U(3.W)
  )

  val rdata = MuxUpTo1H(
    0.U(p.xlen.W),
    Seq(
      fflagsEn    -> fflagsWord(fflags),
      frmEn       -> frmWord(frm),
      fcsrEn      -> fcsrWord(fcsr),
      mstatusEn   -> mstatusWord(mstatus_mie, mstatus_mpie, fs_val, vs_val, ms_val, mstatus_sd),
      misaEn      -> misa,
      mieEn       -> mie,
      mipEn       -> mip,
      mtvecEn     -> mtvec,
      mscratchEn  -> mscratch,
      mepcEn      -> mepc,
      mcauseEn    -> mcause,
      mtvalEn     -> mtval,
      mcontext0En -> mcontext0,
      mcontext1En -> mcontext1,
      mcontext2En -> mcontext2,
      mcontext3En -> mcontext3,
      mcontext4En -> mcontext4,
      mcontext5En -> mcontext5,
      mcontext6En -> mcontext6,
      mcontext7En -> mcontext7,
      mpcEn       -> mpc,
      mspEn       -> msp,
      pmuEn       -> pmu.response.readData,
      mvendoridEn -> mvendorid,
      marchidEn   -> Cat(0.U((p.xlen - 1).W), marchid),
      mimpidEn    -> Cat(0.U((p.xlen - 1).W), mimpid),
      mhartidEn   -> mhartid,
      kisaEn      -> kisa,
      kscm0En     -> kscm(31, 0),
      kscm1En     -> kscm(63, 32),
      kscm2En     -> kscm(95, 64),
      kscm3En     -> kscm(127, 96),
      kscm4En     -> kscm(159, 128)
    ) ++ (if (p.xlen == 32) Seq(mstatushEn -> 0.U(32.W)) else Seq()) ++
      Option
        .when(p.enableRvv) {
          Seq(
            vstartEn.get -> vstartWord(io.rvv.get.vstart),
            vlEn.get     -> io.rvv.get.vl,
            vtypeEn.get  -> io.rvv.get.vtype,
            vxrmEn.get   -> vxrmWord(io.rvv.get.vxrm),
            vxsatEn.get  -> vxsatWord(io.rvv.get.vxsat),
            vlenbEn.get  -> 16.U(32.W) // Vector length in Bytes
          )
        }
        .getOrElse(Seq())
      ++
      Option
        .when(p.enableVme) {
          Seq(
            mtypeEn.get -> io.rvv.get.mtype
          )
        }
        .getOrElse(Seq())
      ++
      Seq(
        tselectEn   -> tselect,
        tdata1En    -> tdata1.asWord,
        tdata2En    -> tdata2,
        tinfoEn     -> tinfo,
        dcsrEn      -> dcsr.asWord,
        dpcEn       -> dpc,
        dscratch0En -> dscratch0,
        dscratch1En -> dscratch1
      )
  )

  val wdata = CsrOp.writeData(req.bits.op, rdata, rs1)

  val mtvec_w = Cat(localWdata(mtvec)(p.programCounterBits - 1, 2), 0.U(2.W))

  val is_csr_write =
    req.valid && CsrOp.writesRegister(req.bits.op, req.bits.rs1)

  // Command is already registered, operand comes from execute-stage register-file/DM mux
  pmu.request.valid   := req.valid && pmuEn
  pmu.request.address := req.bits.index
  pmu.request.op      := req.bits.op
  pmu.request.operand := rs1
  pmu.request.write   := is_csr_write

  val float_dirty_event = Option.when(p.enableFloat) {
    (is_csr_write && (fflagsEn || frmEn || fcsrEn)) ||
    io.float.get.in.fflags.valid ||
    io.float_dirty.get ||
    (if (p.enableVme) io.vme_fp_dirty.get else false.B)
  }

  val rvv_dirty_event = Option.when(p.enableRvv) {
    (is_csr_write && (vstartEn.get || vxrmEn.get || vxsatEn.get || vlEn.get || vtypeEn.get)) ||
    io.rvv.get.fflags.valid ||
    io.rvv_dirty.get ||
    (if (p.enableVme) io.vme_dirty.get else false.B)
  }

  if (p.enableFloat) {
    val mstatus_write_fs = is_csr_write && mstatusEn
    val w_fs             = legalizeFsVs(wdata(14, 13))
    fs.get := Mux(mstatus_write_fs, w_fs, fs.get) | Fill(2, float_dirty_event.get)
  }

  if (p.enableRvv) {
    val mstatus_write_vs = is_csr_write && mstatusEn
    val w_vs             = legalizeFsVs(wdata(10, 9))
    vs.get := Mux(mstatus_write_vs, w_vs, vs.get) | Fill(2, rvv_dirty_event.get)
  }

  if (p.enableVme) {
    val mstatus_write_ms = is_csr_write && mstatusEn
    val w_ms             = wdata(30, 29)
    val ms_post_write    = Mux(mstatus_write_ms, w_ms, ms.get)
    val ms_post_discard  = Mux(io.vme_discard.get, 1.U(2.W), ms_post_write)
    val ms_post_dirty    = ms_post_discard | Fill(2, io.vme_dirty.get)
    ms.get := ms_post_dirty
  }

  val fflags_base = WireDefault(fflags)
  when(req.valid) {
    when(fflagsEn) { fflags_base := wdata(4, 0) }
    when(frmEn) { frm := localWdata(frm)(2, 0) }
    when(fcsrEn) {
      val fcsr_w = localWdata(Cat(frm, fflags))
      fflags_base := fcsr_w(4, 0)
      frm         := fcsr_w(7, 5)
    }
    when(is_csr_write && mstatusEn) {
      mstatus_mie  := wdata(3)
      mstatus_mpie := wdata(7)
    }
    when(mieEn) { mie := wdata & "h888".U }
    when(mtvecEn) { mtvec := mtvec_w }
    // Writes to mstatush are ignored (hardwired zero)
    when(mscratchEn) { mscratch := wdata }
    when(mepcEn) { mepc := mepcWord(localWdata(mepc)) }
    when(mcauseEn) { mcause := wdata }
    when(mtvalEn) { mtval := wdata }
    when(mpcEn) { mpc := wdata }
    when(mspEn) { msp := wdata }
    when(mcontext0En) { mcontext0 := wdata }
    when(mcontext1En) { mcontext1 := wdata }
    when(mcontext2En) { mcontext2 := wdata }
    when(mcontext3En) { mcontext3 := wdata }
    when(mcontext4En) { mcontext4 := wdata }
    when(mcontext5En) { mcontext5 := wdata }
    when(mcontext6En) { mcontext6 := wdata }
    when(mcontext7En) { mcontext7 := wdata }
    when(dscratch0En) { dscratch0 := wdata }
    when(dscratch1En) { dscratch1 := wdata }
    when(tdata1En) { tdata1 := LegalizeTdata1(wdata) }
    when(tdata2En) { tdata2 := wdata }
  }

  if (p.enableRvv) {
    io.rvv.get.vstart_write.valid := req.valid && vstartEn.get
    io.rvv.get.vstart_write.bits  := wdata(log2Ceil(p.rvvVlen) - 1, 0)
    io.rvv.get.vxrm_write.valid   := req.valid && vxrmEn.get
    io.rvv.get.vxrm_write.bits    := wdata(1, 0)
    io.rvv.get.vxsat_write.valid  := req.valid && vxsatEn.get
    io.rvv.get.vxsat_write.bits   := wdata(0)
  }

  val trigger_enabled = tdata1.isTrigger6 && tdata1.m
  val trigger_match   = trigger_enabled && io.dm.current_pc === tdata2

  val entering_debug_mode = (mode =/= CsrMode.Debug) && (io.dm.debug_req || trigger_match)
  val exiting_debug_mode  = (mode === CsrMode.Debug) && (io.dm.resume_req)
  mode := MuxCase(
    mode,
    Seq(
      entering_debug_mode  -> CsrMode.Debug,
      exiting_debug_mode   -> CsrMode.Machine,
      io.bru.in.mode.valid -> io.bru.in.mode.bits
    )
  )
  // debug_mode is registered to break the timing loop to Debug Module.
  // entering_debug_mode blocks dispatch combinationally inside the core.
  io.dm.debug_mode          := (mode === CsrMode.Debug)
  io.dm.entering_debug_mode := entering_debug_mode
  val newCause = MuxCase(
    DebugCause.other,
    Seq(
      (io.dm.debug_req && !io.dm.dcsr_step) -> DebugCause.haltreq,
      trigger_match                         -> DebugCause.trigger,
      io.dm.dcsr_step                       -> DebugCause.step
    )
  )
  dcsr := MuxCase(
    dcsr,
    Seq(
      entering_debug_mode -> {
        val newDcsr = Wire(new Dcsr(p))
        newDcsr          := dcsr
        newDcsr.extcause := false.B
        newDcsr.cause    := newCause
        newDcsr.prv      := 3.U(2.W)
        newDcsr
      },
      (req.valid && dcsrEn) -> wdata.asTypeOf(new Dcsr(p))
    )
  )
  val dpc_value = Mux(newCause === DebugCause.step, io.dm.next_pc, io.dm.current_pc)
  dpc := MuxCase(
    dpc,
    Seq(
      (req.valid && dpcEn) -> wdata,
      entering_debug_mode  -> dpc_value
    )
  )
  io.dm.debug_pc := MuxCase(
    MakeInvalid(UInt(p.fetchAddrBits.W)),
    Seq(
      (req.valid && dpcEn && mode === CsrMode.Debug) -> MakeValid(wdata)
    )
  )

  io.dm.dcsr_step   := dcsr.step
  io.dm.single_step := trigger_enabled

  // High bit of mcause is set for an external interrupt.
  val interrupt = mcause(p.xlen - 1)

  when(io.bru.in.mcause.valid) {
    mcause := io.bru.in.mcause.bits
  }

  when(io.bru.in.mtval.valid) {
    mtval := io.bru.in.mtval.bits
  }

  when(io.bru.in.mepc.valid) {
    mepc := mepcWord(io.bru.in.mepc.bits)
  }

  if (p.enableFloat || p.enableRvv) {
    val float_fflags_valid = if (p.enableFloat) io.float.get.in.fflags.valid else false.B
    val float_fflags_bits  = if (p.enableFloat) io.float.get.in.fflags.bits else 0.U
    val rvv_fflags_valid   = if (p.enableRvv) io.rvv.get.fflags.valid else false.B
    val rvv_fflags_bits    = if (p.enableRvv) io.rvv.get.fflags.bits else 0.U

    when(float_fflags_valid || rvv_fflags_valid) {
      val incoming_fflags = Mux(float_fflags_valid, float_fflags_bits, 0.U) |
        Mux(rvv_fflags_valid, rvv_fflags_bits, 0.U)
      fflags := fflags_base | incoming_fflags
    }.otherwise {
      fflags := fflags_base
    }
  } else {
    fflags := fflags_base
  }

  // Interrupt generation
  val in_debug          = mode === CsrMode.Debug
  val interrupt_pending = (mtip_pending || meip_pending || msip_pending) && mstatus_mie && !in_debug

  io.bru.out.interrupt := interrupt_pending
  val interrupt_bit = BigInt(1) << (p.xlen - 1)
  io.bru.out.interrupt_cause := MuxCase(
    0.U(p.xlen.W),
    Seq(
      meip_pending -> (interrupt_bit | 11).U(p.xlen.W),
      msip_pending -> (interrupt_bit | 3).U(p.xlen.W),
      mtip_pending -> (interrupt_bit | 7).U(p.xlen.W)
    )
  )

  // Trap entry: save mstatus on trap (ecall, ebreak-trap, fault, or interrupt)
  val trap_taken = io.bru.in.mcause.valid
  when(trap_taken) {
    mstatus_mpie := mstatus_mie
    mstatus_mie  := false.B
  }

  // MRET: restore mstatus
  when(io.bru.in.mode.valid) {
    mstatus_mie  := mstatus_mpie
    mstatus_mpie := true.B
  }

  // Forwarding.
  io.bru.out.mode  := mode
  io.bru.out.mepc  := Mux(mepcEn && req.valid, mepcWord(localWdata(mepc)), mepc)
  io.bru.out.mtvec := Mux(mtvecEn && req.valid, mtvec_w, mtvec)

  val frmBypass = MuxCase(
    frm,
    Seq(
      (frmEn && req.valid)  -> localWdata(frm)(2, 0),
      (fcsrEn && req.valid) -> localWdata(Cat(frm, fflags))(7, 5)
    )
  )

  if (p.enableFloat) {
    io.float.get.out.frm := frmBypass
  }
  if (p.enableRvv) {
    io.rvv.get.frm := frmBypass
  }

  io.csr.out.value(0) := io.csr.in.value(12)
  io.csr.out.value(1) := mepc
  io.csr.out.value(2) := mtval
  io.csr.out.value(3) := mcause
  io.csr.out.value(4) := pmu.status.cycle(31, 0)
  io.csr.out.value(5) := pmu.status.cycle(63, 32)
  io.csr.out.value(6) := pmu.status.instret(31, 0)
  io.csr.out.value(7) := pmu.status.instret(63, 32)
  io.csr.out.value(8) := mcontext0

  // Write port.
  io.rd.valid     := req.valid
  io.rd.bits.addr := req.bits.addr
  io.rd.bits.data := rdata

  val fcsr_w = localWdata(fcsr)

  val next_fs =
    if (p.enableFloat) (legalizeFsVs(wdata(14, 13)) | Fill(2, float_dirty_event.get)) else 0.U(2.W)
  val next_vs =
    if (p.enableRvv) (legalizeFsVs(wdata(10, 9)) | Fill(2, rvv_dirty_event.get)) else 0.U(2.W)
  val next_ms =
    if (p.enableVme)
      (Mux(io.vme_discard.get, 1.U(2.W), wdata(30, 29)) | Fill(2, io.vme_dirty.get))
    else 0.U(2.W)
  val next_sd = (next_fs === 3.U) || (next_vs === 3.U) || (next_ms === 3.U)

  val trace_data = MuxUpTo1H(
    wdata,
    Seq(
      mstatusEn -> mstatusWord(wdata(3), wdata(7), next_fs, next_vs, next_ms, next_sd),

      mieEn      -> (wdata & "h888".U),
      mtvecEn    -> mtvec_w,
      mepcEn     -> mepcWord(localWdata(mepc)),
      misaEn     -> misa,
      mstatushEn -> 0.U,
      mipEn      -> mip,
      fflagsEn   -> fflagsWord(wdata),
      frmEn      -> frmWord(localWdata(frm)),
      fcsrEn     -> fcsrWord(fcsr_w),
      tdata1En   -> LegalizeTdata1(wdata).asWord,
      tinfoEn    -> tinfo,
      pmuEn      -> pmu.response.writeData
    ) ++ Seq(
      vstartEn.map(_ -> vstartWord(wdata)),
      vxrmEn.map(_ -> vxrmWord(wdata)),
      vxsatEn.map(_ -> vxsatWord(wdata))
    ).flatten
  )

  io.trace.valid := is_csr_write && CsrAddress.canAccess(req.bits.index, p.xlen, true.B)
  io.trace.addr  := req.bits.index
  io.trace.data  := trace_data

  // Assertions.
  assert(!(req.valid && !io.rs1.valid))
}
