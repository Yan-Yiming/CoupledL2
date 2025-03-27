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

//  val max = io.opcode === ArithmeticData && (io.param === 1.U || io.param === 3.U) // M_XA_MAX || M_XA_MAXU
//  val min = io.opcode === ArithmeticData && (io.param === 0.U || io.param === 2.U) // M_XA_MIN || M_XA_MINU
    val add = io.opcode === ArithmeticData && io.param === 4.U // M_XA_ADD
//  val logic_and = io.opcode === LogicalData && (io.param === 1.U || io.param === 2.U) // M_XA_OR || M_XA_AND
//  val logic_xor = io.opcode === LogicalData && io.param === 0.U // M_XA_XOR || M_XA_OR
//  val logic_xor = io.opcode === LogicalData && io.param === 0.U // M_XA_XOR || M_XA_OR
    val swap = io.opcode === LogicalData && io.param === 3.U

  io.out := Mux(add, io.data1 + io.data2, io.data1)
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

  def data_trans_blockto64(data_block: UInt, offset: UInt): UInt = {
    val off_valid = (offset === 0.U) || (offset === 8.U) || (offset === 16.U) || (offset === 32.U) ||
      (offset === 40.U) || (offset === 48.U) || (offset === 56.U) || (offset === 24.U)
    assert(off_valid, "amo_data not alias")
    val data_ret = (data_block >> (offset * 8.U)).asUInt
    data_ret(63, 0)
  }
  def data_trans_64toblock(data_block: UInt, offset: UInt, new_data: UInt): UInt = {
    val off_valid = (offset === 0.U) || (offset === 8.U) || (offset === 16.U) || (offset === 32.U) ||
      (offset === 40.U) || (offset === 48.U) || (offset === 56.U) || (offset === 24.U)
    assert(off_valid, "amo_data not alias")

    val OFFSET = (offset * 8.U)
    val tmp1 = (1.U << OFFSET) - 1.U
    val tmp2 = data_block & tmp1

    val new_data_block0 = (data_block >> (OFFSET + 64.U)).asUInt
    val new_data_block1 = ((new_data_block0 << 64.U) | new_data).asUInt
    val new_data_block2 = (new_data_block1 << OFFSET).asUInt | tmp2

    new_data_block2
  }

  val amoalu = Module(new AMOALU(64))
  val data1 = RegInit(0.U(64.W))
  val data2 = RegInit(0.U(64.W))
  val data3 = RegInit(0.U(512.W))
  val offset = RegInit(0.U(6.W))
  val opcode = RegInit(0.U(3.W))
  val param = RegInit(0.U(3.W))

  when (io.fromMainPipe.atomicsRequest.valid === true.B) {
    data1 := io.fromMainPipe.atomicsRequest.bits.amo_data
    data2 := data_trans_blockto64(io.fromMainPipe.atomicsRequest.bits.old_data, io.fromMainPipe.atomicsRequest.bits.data_off)
    data3 := io.fromMainPipe.atomicsRequest.bits.old_data
    offset := io.fromMainPipe.atomicsRequest.bits.data_off
    opcode := io.fromMainPipe.atomicsRequest.bits.opcode
    param := io.fromMainPipe.atomicsRequest.bits.param
  }

//  amoalu.io.opcode := io.fromMainPipe.atomicsRequest.bits.opcode
//  amoalu.io.param := io.fromMainPipe.atomicsRequest.bits.param
  amoalu.io.data1 := data1
  amoalu.io.data2 := data2
  amoalu.io.opcode := opcode
  amoalu.io.param := param

  io.toMainPipe.atomicsResult := data_trans_64toblock(data3, offset, amoalu.io.out)
  io.toMainPipe.old_data_back := data1
}