package coupledL2.cchi2chi

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters

class CCHIDummyL2WithMem(implicit p: Parameters) extends Module 
  with HasCCHIOpcodes {
  
  val io = IO(new Bundle {
    val port = Flipped(new CCHIPortIO)
  })

  val id_pool = RegInit(0.U(TXNID_WIDTH.W))
  when(io.port.txreq.fire || io.port.txevt.fire) {
    id_pool := id_pool + 1.U
  }

  val memSize = 1024 
  val memory = Reg(Vec(memSize, UInt(512.W)))
  val indexBits = log2Ceil(memSize)
  val readDataReg = RegInit(0.U(512.W))
  val writeDataReg = RegInit(0.U(256.W))
  val dataBeatCnt = RegInit(2.U(2.W))
  
  def getIdx(addr: UInt) = addr(indexBits + 5, 6)

  val s_dat = RegInit(true.B)
  val s_rsp = RegInit(true.B)
  val s_snp = RegInit(true.B)
  val w_dat = RegInit(true.B)
  val w_rsp = RegInit(true.B)
  val noSchedule = s_dat && s_rsp && s_snp && w_dat && w_rsp

  val valid = RegInit(false.B)
  val addr = RegInit(0.U(ADDR_WIDTH.W))
  val txnID = RegInit(0.U(TXNID_WIDTH.W))
  val opcode = RegInit(0.U(REQ_OPCODE_WIDTH.W))
  val dbid = RegInit(0.U(DBID_WIDTH.W))

  val round = RegInit(false.B)
  round := !round
  io.port.txreq.ready :=  round && !valid
  io.port.txevt.ready := !round && !valid
  val allocReq = io.port.txreq.fire 
  val allocEVT = io.port.txevt.fire
  val alloc = allocReq || allocEVT

  io.port.rxdat.valid := !s_dat
  io.port.rxrsp.valid := !s_rsp
  io.port.rxsnp.valid := false.B

  io.port.txdat.ready := s_rsp && !w_dat
  io.port.txrsp.ready := s_dat && !w_rsp

  val allocBitsAddr   = Mux(allocReq, io.port.txreq.bits.addr,   io.port.txevt.bits.addr)
  val allocBitsTxnID  = Mux(allocReq, io.port.txreq.bits.txnID,  io.port.txevt.bits.txnID)
  val allocBitsOpcode = Mux(allocReq, io.port.txreq.bits.opcode, io.port.txevt.bits.opcode)

  when (alloc) {
    valid  := true.B
    addr   := allocBitsAddr
    txnID  := allocBitsTxnID
    opcode := allocBitsOpcode
    dbid   := id_pool
  }.elsewhen (valid && noSchedule) {
    valid := false.B
  }

  when (alloc) {
    val currentAddr = Mux(allocReq, io.port.txreq.bits.addr, io.port.txevt.bits.addr)
    val currentOpcode = Mux(allocReq, io.port.txreq.bits.opcode, io.port.txevt.bits.opcode)
    val isReadNoSnp = currentOpcode === CCHIREQOpcodes.ReadNoSnp
    val isReadOnce = currentOpcode === CCHIREQOpcodes.ReadOnce
    val isReadOther = currentOpcode === CCHIREQOpcodes.MakeReadUnique || currentOpcode === CCHIREQOpcodes.ReadShared || currentOpcode === CCHIREQOpcodes.ReadUnique 
    val isWriteNoSnp = currentOpcode === CCHIREQOpcodes.WriteNoSnpFull || currentOpcode === CCHIREQOpcodes.WriteNoSnpPtl

    when (isReadNoSnp || isReadOnce) {
      dataBeatCnt := 2.U
      val readData = memory(getIdx(currentAddr))
      readDataReg := readData
      s_dat := false.B
      s_rsp := true.B
      s_snp := true.B
      w_dat := true.B
      w_rsp := true.B
    }

    when (isReadOther) {
      dataBeatCnt := 2.U
      val readData = memory(getIdx(currentAddr))
      readDataReg := readData
      s_dat := false.B
      s_rsp := true.B
      s_snp := true.B
      w_dat := true.B
      w_rsp := false.B
    }

    when (isWriteNoSnp) {
      dataBeatCnt := 2.U
      s_dat := true.B
      s_rsp := false.B
      s_snp := true.B
      w_dat := false.B
      w_rsp := true.B
    }
  }

  io.port.rxdat.bits.txnID := txnID
  io.port.rxdat.bits.dbID := dbid
  io.port.rxdat.bits.opcode := CCHIDATDownOpcodes.CompData
  io.port.rxdat.bits.data := Mux(dataBeatCnt === 2.U, readDataReg(255, 0), readDataReg(511, 256))
  when (io.port.rxdat.fire) {
    when (dataBeatCnt === 2.U) {
      dataBeatCnt := 1.U
    }.elsewhen (dataBeatCnt === 1.U) {
      dataBeatCnt := 0.U
      s_dat := true.B
    }
  }

  when (io.port.txrsp.fire) {
    w_rsp := true.B
  }

  io.port.rxrsp.bits.txnID := txnID
  io.port.rxrsp.bits.opcode := MuxLookup(opcode, CCHIRSPDownOpcodes.Comp)(Seq(
    CCHIREQOpcodes.WriteNoSnpFull -> CCHIRSPDownOpcodes.CompDBIDResp,
    CCHIREQOpcodes.WriteNoSnpPtl  -> CCHIRSPDownOpcodes.CompDBIDResp
  ))
  io.port.rxrsp.bits.dbID := dbid
  when (io.port.rxrsp.fire) {
    s_rsp := true.B
  }

  when (io.port.txdat.fire) {
    when (dataBeatCnt === 2.U) {
      writeDataReg := io.port.txdat.bits.data
      dataBeatCnt := 1.U
    }.elsewhen (dataBeatCnt === 1.U) {
      memory(getIdx(addr)) := Cat(io.port.txdat.bits.data, writeDataReg)
      dataBeatCnt := 0.U
      w_dat := true.B
    }
  }

  io.port.rxsnp.bits.txnID := DontCare
  io.port.rxsnp.bits.opcode := DontCare
  io.port.rxsnp.bits.addr := DontCare
  io.port.rxsnp.bits.ns := DontCare
  io.port.rxsnp.bits.traceTag := DontCare

  io.port.rxrsp.bits.resp := DontCare
  io.port.rxrsp.bits.respErr := DontCare
  io.port.rxrsp.bits.cBusy := DontCare
  io.port.rxrsp.bits.traceTag := DontCare

  io.port.rxdat.bits.resp := DontCare
  io.port.rxdat.bits.respErr := DontCare
  io.port.rxdat.bits.dataSource := DontCare
  io.port.rxdat.bits.cBusy := DontCare
  io.port.rxdat.bits.traceTag := DontCare
}