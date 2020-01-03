package cvmorph

import scala.annotation.tailrec

/** *
 *
 * @param lines
 */
class ChiselVerilogList(val lines: List[String]) {

  import ChiselVerilogList._

  val flops: Set[String] = extractFlops(lines)
  val (assignMap, singleRhsMap) = getAssignMap(lines, flops)
  val newlines: List[String] = replaceVerilogList(lines, assignMap, singleRhsMap, flops)
  // debugPrint(flops)
}

object ChiselVerilogList {

  import AssignMap._
  import VerilogExpr._

  def apply(lines: List[String]) = new ChiselVerilogList(lines)

  def extractFlops(lines: List[String]): Set[String] = {
    (for (line <- lines) yield extractFlopFromLine(line)).flatten.toSet
  }

  def debugPrint(ss: Set[String]): Unit = ss.foreach { x => println(x) }

  def getAssignMap(lines: List[String], flops: Set[String]): (Map[String, String], Map[String, Boolean]) = {
    val (initMap, singleRhsMap) = assignMapFromVerilogList(lines)
    //initMap.foreach{case (k, v) => println(k + " = " + v + " // " + singleRhsMap(k))}
    val finalMap = refineMap(initMap, singleRhsMap, flops)
    //initMap.foreach{case (k, v) => println(k + " = " + v + " // " + singleRhsMap(k) + "-->" + finalMap(k))}
    (finalMap, singleRhsMap)
  }

  def replaceVerilogList(lines: List[String],
                         m: Map[String, String],
                         sm: Map[String, Boolean],
                         flops: Set[String]): List[String] = {
    (for (line <- lines) yield replaceVerilogLine(line, m, sm, flops)).flatten
  }

  /**
   * Modify the assign statement using the (LHS, RHS) map
   * Two possibilities:
   * 1) Return a list of single element ("old statement") if the old statement is unchanged due to
   * a) nothing to changed for
   * b) not an assignment statement
   * c) (LHS, RHS) is missing in map
   * 2) Return a list of ("commented out old statement", "new statement" if the statement is modified.   *
   *
   * @param s
   * @param m
   * @param flops
   * @return
   */
  def replaceAssignLine(s: String,
                        m: Map[String, String],
                        sm: Map[String, Boolean],
                        flops: Set[String]):
  (Boolean, String) = {
    extractAssignFromLine(s) match {
      case Some((lh, rh)) => {
        if (exprIsFinal(rh)(flops)) (false, s) else {
          val newRh = replaceTemps(rh.trim, m, sm)
          (true, "  assign " + lh + " = " + newRh + ";")
        }
      }
      case None => (false, s)
    }
  }

  /**
   * replace the temp signals in the nonBlocking statement.
   *
   * @param s
   * @param m
   * @param sm
   * @param flops
   * @return
   */
  def replaceNonBlockingLine(s: String,
                             m: Map[String, String],
                             sm: Map[String, Boolean],
                             flops: Set[String]):
  (Boolean, String) = {
    extractNonBlockingLine(s) match {
      case Some((lh, e)) => {
        if (exprIsFinal(e)(flops)) (false, s) else {
          val newRh = replaceTemps(e.trim, m, sm)
          (true, lh + "<= " + newRh + ";")
        }
      }
      case None => (false, s)
    }
  }

  /**
   * replace the temp signals in a if block statement
   *
   * @param s
   * @param m
   * @param sm
   * @param flops
   * @return
   */
  def replaceIfLine(s: String,
                    m: Map[String, String],
                    sm: Map[String, Boolean],
                    flops: Set[String]):
  (Boolean, String) = {
    extractIfLine(s) match {
      case Some((lh, e, rh)) => {
        if (exprIsFinal(e)(flops)) (false, s) else {
          val newRh = replaceTemps(e.trim, m, sm)
          (true, lh + newRh + rh)
        }
      }
      case None => (false, s)
    }
  }

  /**
   * classify the verilog lines and replace the temp signals if needed.
   *
   * @param s
   * @param m
   * @param sm
   * @param flops
   * @return
   */
  def replaceVerilogLine(s: String,
                         m: Map[String, String],
                         sm: Map[String, Boolean],
                         flops: Set[String]): List[String] = {
    val (r, s1) =
      if (isAssignStatement(s)) replaceAssignLine(s, m, sm, flops)
      else if (isNonBlockingLine(s)) replaceNonBlockingLine(s, m, sm, flops)
      else if (isIfLine(s)) replaceIfLine(s, m, sm, flops)
      else (false, s)

    if (r == true) {
      val s2 = prettyMultiLineFormat(s1, max = 80, tab = 2)
      "//" + s :: s2 ::: Nil
    } else {
      List(s)
    }
  }

  /** *
   * split a long statement into multiple lines with sensible indentation for readability.
   * always start a new line when "(" is seen except
   * 1) a continuous stream of "(".
   * 2)
   *
   * @param s   : the string to be split
   * @param max : the maxinum length of a line
   * @param tab : the white space indentation per "("
   * @return: a list of lines
   */
  def prettyMultiLineFormat(s: String, max: Int, tab: Int): List[String] = {


    val fixIndent = extractIndent(s)

    def loop(l: List[String], leftp: Int, curLine: List[String], accLines: List[String]): List[String] = {
      require(leftp >= 0)

      def leftParenthesisWithSpace: String = " " * (tab - 1)

      val curLineString = " " * fixIndent + curLine.reverse.mkString("")
      l match {
        case Nil => curLineString :: accLines
        case ("~" :: " " :: "(" :: xs) => loop("~" :: "(" :: xs, leftp, curLine, accLines)
        case ("^" :: " " :: "(" :: xs) => loop("^" :: "(" :: xs, leftp, curLine, accLines)
        case (x :: xs) => {
          //println("prettyMultiLineFormat: " + "curLine=" + curLine.reverse.mkString("") + "# x=" + x)
          // always start a new line when "(" is seen except
          // 1) it is a continuous stream of "(", such as "( ( ("
          // 2) the expression has no nested parenthesis, so we can put a simple binary expression in one line
          if (x == "(") {
            val futureExprIsShort = (curLineString.length + pairedSubExprLength(xs) <= max)
            //println("futureExprSize=" + (curLineString.length + pairedSubExprLength(xs)) + " nestedParenthesis=" + nestedParenthesis(xs))
            if (curLine.isEmpty ||
              List("(", "^", "~").contains(curLine.head.trim) ||
              !nestedParenthesis(xs) && futureExprIsShort) {
              loop(xs, leftp + 1, x + leftParenthesisWithSpace :: curLine, accLines)
            } else {
              //println("start new line")
              val newline = List(" " * leftp * tab + "(" + leftParenthesisWithSpace)
              loop(xs, leftp + 1, newline, curLineString :: accLines)
            }
          } else if (x == ")") {
            //println(x + ":         accumulate the left parenthesis, continue")
            loop(xs, leftp - 1, " " + x :: curLine, accLines)
          } else {
            //println(x + ":          continue")
            loop(xs, leftp, x :: curLine, accLines)
          }
        }
      }
    }

    val inputLine = "//" + s
    if (s.length <= max) { // short-circuit
      List(s)
    } else {
      val list = parseVerilogExpr(s)
      //      loop(list, leftp = 0, List(), List(inputLine)).reverse
      loop(list, leftp = 0, List(), List()).reverse
    }
  }

  private def nestedParenthesis(list: List[String]): Boolean = {
    @tailrec
    def loop(l: List[String]): Boolean = {
      l match {
        case (x :: xs) => {
          if (x.trim == "(") true
          else if (x.trim == ")") false
          else loop(xs)
        }
        case Nil => false
      }
    }

    loop(list)
  }

  private def pairedSubExprLength(list: List[String]): Int = {
    //@tailrec
    def loop(l: List[String], leftp: Int): List[String] = {
      l match {
        case Nil => Nil
        case (x :: xs) => {
          if (x.trim == ")" && leftp == 1) Nil
          else if (x.trim == ")") x :: loop(xs, leftp - 1)
          else if (x.trim == "(") x :: loop(xs, leftp + 1)
          else x :: loop(xs, leftp)
        }
      }
    }

    loop(list, 1).mkString("").length
  }


}