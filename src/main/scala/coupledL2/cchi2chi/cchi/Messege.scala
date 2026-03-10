package coupledL2.cchi2chi

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import coupledL2.L2ParamKey

trait HasCCHIMsgParameters {

  val DEFAULT_CONFIG = Map(
    "TXNID_WIDTH"              -> 12,

    "ADDR_WIDTH"               -> 48,
    "REQ_OPCODE_WIDTH"         -> 6,
    "SIZE_WIDTH"               -> 3,
    "ORDER_WIDTH"              -> 2,
    "MEMATTR_WIDTH"            -> 4,
    "EXCL_WIDTH"               -> 1,
    "EXPCOMPSTASH_WIDTH"       -> 1,
    
    "EVT_OPCODE_WIDTH"         -> 2,
    "NS_WIDTH"                 -> 1,
    "TRACETAG_WIDTH"           -> 1,

    "DBID_WIDTH"               -> 12,
    "RSP_OPCODE_WIDTH"         -> 3,
    "RESPERR_WIDTH"            -> 2,
    "RESP_WIDTH"               -> 3,
    "CBUSY_WIDTH"              -> 3,
    
    "DAT_OPCODE_WIDTH"         -> 2,
    "DATASOURCE_WIDTH"         -> 5,
    "DATA_WIDTH"               -> 256,
    "BE_WIDTH"                 -> 32,

    "SNP_OPCODE_WIDTH"         -> 2,
    "SNP_ADDR_WIDTH"           -> 45
  )

  val params = DEFAULT_CONFIG

  def CONFIG(key: String): Int = params.getOrElse(key, 0)

  def TXNID_WIDTH      = CONFIG("TXNID_WIDTH")
  def ADDR_WIDTH       = CONFIG("ADDR_WIDTH")
  def DATA_WIDTH       = CONFIG("DATA_WIDTH")
  def BE_WIDTH         = CONFIG("BE_WIDTH")
  def SIZE_WIDTH       = CONFIG("SIZE_WIDTH")
  
  def REQ_OPCODE_WIDTH = CONFIG("REQ_OPCODE_WIDTH")
  def EVT_OPCODE_WIDTH = CONFIG("EVT_OPCODE_WIDTH")
  def RSP_OPCODE_WIDTH = CONFIG("RSP_OPCODE_WIDTH")
  def SNP_OPCODE_WIDTH = CONFIG("SNP_OPCODE_WIDTH")
  def DAT_OPCODE_WIDTH = CONFIG("DAT_OPCODE_WIDTH")

  def MEMATTR_WIDTH    = CONFIG("MEMATTR_WIDTH")
  def ORDER_WIDTH      = CONFIG("ORDER_WIDTH")
  def RESPERR_WIDTH    = CONFIG("RESPERR_WIDTH")
  def RESP_WIDTH       = CONFIG("RESP_WIDTH")
  def CBUSY_WIDTH      = CONFIG("CBUSY_WIDTH")
  def DATASOURCE_WIDTH = CONFIG("DATASOURCE_WIDTH")
  def DBID_WIDTH       = CONFIG("DBID_WIDTH")

  def SNP_ADDR_WIDTH   = CONFIG("SNP_ADDR_WIDTH")
}

abstract class CCHIBundle(implicit val p: Parameters) extends Bundle with HasCCHIMsgParameters

class CCHIREQ(implicit p: Parameters) extends CCHIBundle {
  val txnID        = UInt(TXNID_WIDTH.W)      // 事务标识
  val opcode       = UInt(REQ_OPCODE_WIDTH.W) // 请求类型
  val size         = UInt(SIZE_WIDTH.W)       // 传输大小
  val addr         = UInt(ADDR_WIDTH.W)       // 地址 (48位)
  val ns           = Bool()                   // 非安全位
  val order        = UInt(ORDER_WIDTH.W)      // 顺序属性
  val memAttr      = UInt(MEMATTR_WIDTH.W)    // 内存属性
  val excl         = Bool()                   // 独占访问标识
  val expCompStash = Bool()                   // 预取完成确认标识
}

class CCHIEVT(implicit p: Parameters) extends CCHIBundle {
  val txnID      = UInt(TXNID_WIDTH.W)      // 事务标识
  val opcode     = UInt(EVT_OPCODE_WIDTH.W) // 驱逐类型
  val addr       = UInt(ADDR_WIDTH.W)       // 地址 (48位)
  val ns         = Bool()                   // 非安全位
  val memAttr    = Bool()                   // 仅含 Allocate 属性
  val traceTag   = Bool()                   // 追踪标记
  val allowRetry = Bool()                   // 重试许可 (可选)
}

class CCHISNP(implicit p: Parameters) extends CCHIBundle {
  val txnID    = UInt(TXNID_WIDTH.W)      // 事务标识
  val opcode   = UInt(SNP_OPCODE_WIDTH.W) // 监听类型
  val addr     = UInt(SNP_ADDR_WIDTH.W)   // 监听地址 (通常为 ADDR_WIDTH - 3)
  val ns       = Bool()                   // 非安全位
  val traceTag = Bool()                   // 追踪标记
}

class CCHIRSPDown(implicit p: Parameters) extends CCHIBundle {
  val txnID    = UInt(TXNID_WIDTH.W)      // 由上游决定 
  val dbID     = UInt(DBID_WIDTH.W)       // 由下游决定 
  val opcode   = UInt(RSP_OPCODE_WIDTH.W) // 0, 2 or 3 (总宽为3) 
  val respErr  = UInt(RESPERR_WIDTH.W)    // 2位 
  val resp     = UInt(RESP_WIDTH.W)       // 3位 
  val cBusy    = UInt(CBUSY_WIDTH.W)      // 3位 
  val traceTag = Bool()                   // 1位 
}

class CCHIRSPUp(implicit p: Parameters) extends CCHIBundle {
  val txnID    = UInt(TXNID_WIDTH.W)      // 由下游决定
  val opcode   = UInt(1.W)                // 0 or 1 (总宽为1) 
  val respErr  = UInt(RESPERR_WIDTH.W)    // 2位 
  val resp     = UInt(RESP_WIDTH.W)       // 3位 
  val traceTag = Bool()                   // 1位 
}

class CCHIDATDown(implicit p: Parameters) extends CCHIBundle {
  val txnID      = UInt(TXNID_WIDTH.W)      // 由上游决定 
  val dbID       = UInt(DBID_WIDTH.W)       // 由下游决定 
  val opcode     = UInt(1.W)                // 总是 0 (CompData) 
  val respErr    = UInt(RESPERR_WIDTH.W)    // 2位 
  val resp       = UInt(RESP_WIDTH.W)       // 3位 
  val dataSource = UInt(DATASOURCE_WIDTH.W) // 5位 
  val cBusy      = UInt(CBUSY_WIDTH.W)      // 3位 
  val data       = UInt(DATA_WIDTH.W)       // 256位 
  val traceTag   = Bool()                   // 1位 
}

/**
  * 11.1.5 DAT 通道 - 2. 由上游出发的 DAT 通道 (Upstream to Downstream)
  * [cite: 768-769]
  */
class CCHIDATUp(implicit p: Parameters) extends CCHIBundle {
  val txnID    = UInt(TXNID_WIDTH.W)
  val opcode   = UInt(DAT_OPCODE_WIDTH.W)
  val respErr  = UInt(RESPERR_WIDTH.W)
  val resp     = UInt(RESP_WIDTH.W)
  val data     = UInt(DATA_WIDTH.W)
  val be       = UInt(BE_WIDTH.W)
  val traceTag = Bool()
}

class CCHIPortIO(implicit p: Parameters) extends CCHIBundle {
  val txevt = Decoupled(new CCHIEVT)
  val txreq = Decoupled(new CCHIREQ)
  
  val rxsnp = Flipped(Decoupled(new CCHISNP))
  
  val txrsp = Decoupled(new CCHIRSPUp)  
  val rxrsp = Flipped(Decoupled(new CCHIRSPDown))
  
  val txdat = Decoupled(new CCHIDATUp)
  val rxdat = Flipped(Decoupled(new CCHIDATDown))
}