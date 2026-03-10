package coupledL2.cchi2chi

import chisel3._
import chisel3.util._

trait HasCCHIOpcodes extends HasCCHIMsgParameters {

  object CCHIEVTOpcodes {
    val Evict          = "h00".U(2.W)
    val WriteBackFull  = "h02".U(2.W)
  }

  object CCHIREQOpcodes {
    val StashShared      = "h00".U(6.W)
    val StashUnique      = "h01".U(6.W)
    val ReadNoSnp        = "h02".U(6.W)
    val ReadOnce         = "h03".U(6.W)
    val ReadShared       = "h04".U(6.W)
    val WriteNoSnpPtl    = "h08".U(6.W)
    val WriteNoSnpFull   = "h09".U(6.W)
    val WriteUniquePtl   = "h0A".U(6.W)
    val WriteUniqueFull  = "h0B".U(6.W)
    val CleanShared      = "h0C".U(6.W)
    val CleanInvalid     = "h0D".U(6.W)
    val MakeInvalid      = "h0E".U(6.W)
    val ReadUnique       = "h10".U(6.W)
    val MakeUnique       = "h12".U(6.W)
    val MakeReadUnique   = "h13".U(6.W)

    val AtomicLoad       = "h20".U(6.W) // 范围 0x20-0x27
    val AtomicStore      = "h28".U(6.W) // 范围 0x28-0x2F
    val AtomicSwap       = "h30".U(6.W)
    val AtomicCompare    = "h31".U(6.W)
  }

  object CCHISNPOpcodes {
    val SnpMakeInvalid = "h00".U(2.W)
    val SnpToInvalid   = "h01".U(2.W)
    val SnpToShared    = "h02".U(2.W)
    val SnpToClean     = "h03".U(2.W)
  }

  object CCHIRSPDownOpcodes {
    val CompStash     = "h00".U(3.W)
    val Comp          = "h01".U(3.W)
    val DBIDResp      = "h02".U(3.W)
    val CompDBIDResp  = "h03".U(3.W)
    val CompCMO       = "h04".U(3.W)
  }

  object CCHIRSPUpOpcodes {
    val CompAck = "h00".U(1.W)
    val SnpResp = "h01".U(1.W)
  }

  object CCHIDATDownOpcodes {
    val CompData = "h00".U(1.W)
  }

  object CCHIDATUpOpcodes {
    val NonCopyBackWrData = "h00".U(2.W)
    val CopyBackWrData    = "h02".U(2.W)
    val SnpRespData       = "h03".U(2.W)
  }
}