package cvmorph

import cvmorph.AssignMap._
import org.scalatest.FunSuite



class AssignMapSuite extends FunSuite {
  val am = Map("_T_1" -> "(foo == 1) | (bar == 1)",
    "_T_2" -> "xxx == 2",
    "_T_3" -> "a[3:0] | b[3:0]",
    "_T_4" -> "^a[3:0]")

  val sm = Map("_T_1" -> false,
    "_T_2" -> false,
    "_T_3" -> false,
    "_T_4" -> true)

  test("replaceTemps: ") {
    assert(replaceTemps("", am, sm) === "")
    assert(replaceTemps("foo_T_1", am, sm) === "foo_T_1")
    assert(replaceTemps("_T_3", am, sm) === "(a[3:0] | b[3:0])")
    //assert(replaceTemps("_T_3[0]", am, sm) === "{a[3:0] | b[3:0]}[0]")
    assert(replaceTemps("_T_3[0]", am, sm) === "_T_3[0]")  // no replacement since VCS can not handle it
  }

}
/*
class VerilogExprSuite extends FunSuite {
  test("normalizeWhiteSpace: 1") {
    assert(normalizeExpr("foo|bar") === "foo | bar")
  }
  test("normalizeWhiteSpace: 2") {
    assert(normalizeExpr("foo||bar") === "foo || bar")
  }
  test("normalizeWhiteSpace: 3") {
    assert(normalizeExpr("    foo   ||     bar     ") === "foo || bar")
  }
  test("normalizeWhiteSpace: 4") {
    assert(normalizeExpr("  foo [0: 10]  | bar [  11  :    20   ]") === "foo[0:10] | bar[11:20]")
  }

}

 */
