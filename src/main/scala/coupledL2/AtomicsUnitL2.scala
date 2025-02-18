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
  val minXLen = 32
  val widths = (0 to log2Ceil(operandBits / minXLen)).map(minXLen << _)

  val io = IO(new Bundle {
    val mask = Input(UInt((operandBits/8).W))
    val opcode = Input(Bits(3.W))
	  val param = Input(Bits(3.W))
    val lhs = Input(Bits(operandBits.W))
    val rhs = Input(Bits(operandBits.W))
    val out = Output(Bits(operandBits.W))
    val out_unmasked = Output(Bits(operandBits.W))
  })

  val max = io.opcode === ArithmeticData && (io.param === 1.U || io.param === 3.U) // M_XA_MAX || M_XA_MAXU
  val min = io.opcode === ArithmeticData && (io.param === 0.U || io.param === 2.U) // M_XA_MIN || M_XA_MINU
  val add = io.opcode === ArithmeticData && io.param === 4.U // M_XA_ADD
  val logic_and = io.opcode === LogicalData && (io.param === 1.U || io.param === 2.U) // M_XA_OR || M_XA_AND
  val logic_xor = io.opcode === LogicalData && io.param === 0.U // M_XA_XOR || M_XA_OR

  val adder_out = {
    // partition the carry chain to support sub-xLen addition
    val mask = ~(0.U(operandBits.W) +: widths.init.map(w => !io.mask(w/8-1) << (w-1))).reduce(_|_)
    (io.lhs & mask) + (io.rhs & mask)
  }

  val less = {
    // break up the comparator so the lower parts will be CSE'd
    def isLessUnsigned(x: UInt, y: UInt, n: Int): Bool = {
      if (n == minXLen) x(n-1, 0) < y(n-1, 0)
      else x(n-1, n/2) < y(n-1, n/2) || x(n-1, n/2) === y(n-1, n/2) && isLessUnsigned(x, y, n/2)
    }

    def isLess(x: UInt, y: UInt, n: Int): Bool = {
      val signed = {
		    io.opcode === ArithmeticData && (io.param === 0.U || io.param === 1.U)
      }
      Mux(x(n-1) === y(n-1), isLessUnsigned(x, y, n), Mux(signed, x(n-1), y(n-1)))
    }

    PriorityMux(widths.reverse.map(w => (io.mask(w/8/2), isLess(io.lhs, io.rhs, w))))
  }

  val minmax = Mux(Mux(less, min, max), io.lhs, io.rhs)
  val logic =
    Mux(logic_and, io.lhs & io.rhs, 0.U) |
    Mux(logic_xor, io.lhs ^ io.rhs, 0.U)
  val out =
    Mux(add,                    adder_out,
    Mux(logic_and || logic_xor, logic,
                                minmax))

  val wmask = FillInterleaved(8, io.mask)
  io.out := wmask & out | ~wmask & io.lhs
  io.out_unmasked := out
}

class AtomicsUnitL2(implicit p: Parameters) extends TL2CHIL2Module {
	val io = IO(new Bundle() {    
    val fromMainPipe = new Bundle() {
      val atomicsRequest = Flipped(ValidIO(new AtomicsReq()))
    }

		val toMainPipe = new Bundle() {
      val atomicsResult = Output(UInt(64.W))
    }
	})

	val amoalu = Module(new AMOALU(64))
	amoalu.io.mask := io.fromMainPipe.atomicsRequest.bits.amo_mask
	amoalu.io.opcode := io.fromMainPipe.atomicsRequest.bits.opcode
	amoalu.io.param := io.fromMainPipe.atomicsRequest.bits.param
	amoalu.io.lhs := io.fromMainPipe.atomicsRequest.bits.old_data
	amoalu.io.rhs := io.fromMainPipe.atomicsRequest.bits.amo_data

  io.toMainPipe.atomicsResult := amoalu.io.out
}