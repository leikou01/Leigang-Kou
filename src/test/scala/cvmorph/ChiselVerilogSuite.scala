package cvmorph

import cvmorph.VerilogExpr.{exprIsFinal, extractAssignFromLine, parseVerilogExpr}
import cvmorph.ChiselVerilogList._
import org.scalatest.FunSuite

class ChiselVerilogSuite extends FunSuite {


  val flops = Set("_T_10", "_T_20")

  test("parseVerilogExpr: vanilla binary op 1") {
    assert(parseVerilogExpr("a+b") === List("a", "+", "b"))
  }

  test("parseVerilogExpr: vanilla unary op 1") {
    assert(parseVerilogExpr("~b") === List("~", "b"))
  }

  test("parseVerilogExpr: vanilla empty space 1") {
    assert(parseVerilogExpr("a  + b") === List("a  ", "+", " b"))
  }

  test("exprIsFina : vanilla empty space 1") {
    assert(exprIsFinal("a  + b") === true)
    assert(exprIsFinal("a  + b")(flops) === true)
    assert(exprIsFinal("_T_10  + _T_20")(flops) === true)
    assert(exprIsFinal("_T_10  + _T_30")(flops) === false)
    assert(exprIsFinal("_T_1[7:0]") === true)
  }

  val s1 = "  assign _T_54 = ((((abc_mshrs_0_io_schedule_valid & (mshr_stall_0 == 1'h0)) & (sourceA_io_req_ready | (abc_mshrs_0_io_schedule_bits_a_valid == 1'h0))) & (sourceC_io_req_ready | (abc_mshrs_0_io_schedule_bits_c_valid == 1'h0))) & (sourceD_io_req_ready | (abc_mshrs_0_io_schedule_bits_d_valid == 1'h0))) & (sourceE_io_req_ready | (abc_mshrs_0_io_schedule_bits_e_valid == 1'h0));"
  val s2 = prettyMultiLineFormat(s1, max = 80, tab = 2)
  s2.foreach(println)
  /*
    test("normalizeBracket: only remove inner white space") {
      assert(normalizeBracket(" foo [ 3: 0  ] ") === " foo[3:0] ")
    }

    test("delimitWithWhiteSpace: 1") {
      assert(delimitWithWhiteSpace("a       +         b") === "a + b")
      assert(delimitWithWhiteSpace("a[1:0]") === "a [ 1 : 0 ]")
      assert(delimitWithWhiteSpace("{a,b,    c}") === "{ a , b , c }")
    }

    test("finalizeAssignRhs: 1") {
      assert(finalizeAssignRhs("foo == 2") === "( foo == 2 )")
    }
  */

  test("extractAssignFromLine: simple assign statement 1") {
    assert(extractAssignFromLine("  assign foo = bar ; ") === Some("foo", "bar"))
    assert(extractAssignFromLine("assign foo=bar; ") === Some("foo", "bar"))
    assert(extractAssignFromLine("assign {foo, bar} = a[1:0]; ") === None)
  }
}
