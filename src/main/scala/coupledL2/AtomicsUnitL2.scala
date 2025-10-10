/** *************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 * http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 * *************************************************************************************
 */

package coupledL2

import chisel3._
import chisel3.util._
import utility._
import coupledL2.tl2tl._
import coupledL2.tl2chi._
import freechips.rocketchip.tilelink.TLMessages._
import freechips.rocketchip.tilelink.ClientStates._
import freechips.rocketchip.tilelink.MemoryOpCategories._
import freechips.rocketchip.tilelink.TLPermissions._
import freechips.rocketchip.tilelink._

import org.chipsalliance.cde.config.Parameters

class AMOALU(operandBits: Int) extends Module
//  with MemoryOpConstants
{
  val io = IO(new Bundle {
    val opcode = Input(UInt(3.W))
    val param = Input(UInt(3.W))
    val data1 = Input(UInt(operandBits.W))
    val data2 = Input(UInt(operandBits.W))
    val out = Output(UInt(operandBits.W))
  })

  val max   = io.opcode === ArithmeticData && io.param === 1.U
  val maxu  = io.opcode === ArithmeticData && io.param === 3.U
  val min   = io.opcode === ArithmeticData && io.param === 0.U
  val minu  = io.opcode === ArithmeticData && io.param === 2.U
  val add   = io.opcode === ArithmeticData && io.param === 4.U
  val xor   = io.opcode === LogicalData && io.param === 0.U
  val or    = io.opcode === LogicalData && io.param === 1.U
  val and   = io.opcode === LogicalData && io.param === 2.U
  val swap  = io.opcode === LogicalData && io.param === 3.U

  io.out := ParallelPriorityMux(Seq(
    max   -> Mux(io.data1.asSInt > io.data2.asSInt, io.data1, io.data2),
    maxu  -> Mux(io.data1 > io.data2, io.data1, io.data2),
    min   -> Mux(io.data1.asSInt < io.data2.asSInt, io.data1, io.data2),
    minu  -> Mux(io.data1 < io.data2, io.data1, io.data2),
    xor   -> (io.data1 ^ io.data2),
    or    -> (io.data1 | io.data2),
    add   -> (io.data1 + io.data2),
    swap  -> (io.data1)
  ))
}

class AtomicsUnitL2(implicit p: Parameters) extends TL2CHIL2Module {
  val io = IO(new Bundle() {
    val fromMainPipe = new Bundle() {
      val atomicsRequest = Flipped(ValidIO(new AtomicsReq()))
    }

    val toMainPipe = new Bundle() {
      val atomicsResult = Output(UInt(512.W))
      val old_data_back = Output(UInt(64.W))
    }
  })

  def data_trans_blocktoN(data_block: UInt, offset: UInt, size: UInt): UInt = {
    val N = Mux(size === 2.U, 32.U, 64.U)
    val data_ret = (data_block >> (offset * 8.U)).asUInt
//    assert((size === 2.U || size === 3.U), "size misalias")
    assert((offset % (N / 8.U).asUInt) === 0.U, "amo_data not alias")

    Mux(N === 64.U, data_ret(63, 0), Fill(2, data_ret(31, 0)))
  }

  def data_trans_Ntoblock(data_block: UInt, offset: UInt, new_data: UInt, size: UInt): UInt = {
    val N = Mux(size === 2.U, 32.U, 64.U)
    val OFFSET = (offset * 8.U)
    val data = Mux(N === 64.U, new_data(63, 0), new_data(31, 0))
//    assert((size === 2.U || size === 3.U), "size misalias")
    assert((offset % (N / 8.U).asUInt) === 0.U, "amo_data not alias")

    (((((data_block >> (OFFSET + N))) << N) | data) << OFFSET) | (data_block & ((1.U << OFFSET) - 1.U))
  }

  val amoalu = Module(new AMOALU(64))
  val data1 = RegInit(0.U(64.W))
  val data2 = RegInit(0.U(64.W))
  val data3 = RegInit(0.U(512.W))
  val offset = RegInit(0.U(6.W))
  val opcode = RegInit(0.U(3.W))
  val param = RegInit(0.U(3.W))
  val size = RegInit(0.U(2.W))

  when (io.fromMainPipe.atomicsRequest.valid === true.B) {
    data1 := io.fromMainPipe.atomicsRequest.bits.amo_data
    data2 := data_trans_blocktoN(io.fromMainPipe.atomicsRequest.bits.old_data, io.fromMainPipe.atomicsRequest.bits.data_off,
      io.fromMainPipe.atomicsRequest.bits.amo_lgsize)
    data3 := io.fromMainPipe.atomicsRequest.bits.old_data
    offset := io.fromMainPipe.atomicsRequest.bits.data_off
    opcode := io.fromMainPipe.atomicsRequest.bits.opcode
    param := io.fromMainPipe.atomicsRequest.bits.param
    size := io.fromMainPipe.atomicsRequest.bits.amo_lgsize
  }

  amoalu.io.data1 := data1
  amoalu.io.data2 := data2
  amoalu.io.opcode := opcode
  amoalu.io.param := param

  io.toMainPipe.atomicsResult := data_trans_Ntoblock(data3, offset, amoalu.io.out, size)
  io.toMainPipe.old_data_back := data1
}