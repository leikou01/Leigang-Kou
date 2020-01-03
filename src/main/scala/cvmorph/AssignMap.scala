package cvmorph

import scala.util.matching.Regex

object AssignMap {

  import VerilogExpr._

  /**
   * extract the initial mapping of (LHS -> RHS) from the verilog
   *
   * @param lines : a list of verilog lines
   * @return a map of assign statement (LHS -> RHS), and
   *         a map of whether or not RHS is a single expression.
   */
  def assignMapFromVerilogList(lines: List[String]):
  (Map[String, String], Map[String, Boolean]) = {
    // "for comprehension" yields a List[Option[(String, String)]]
    // For a sequence of Some and None elements, flatten drops the None elements
    val m = (for (s <- lines) yield extractAssignFromLine(s)).flatten.toMap

    val singleRhsMap = for ((k, v) <- m) yield (k.trim, chiselExprIsSingle(v.trim))
    val initMap = for ((k, v) <- m) yield (k.trim, v.trim)

    // m.foreach { case (k, v) => println(k + " = " + v + " // " + singleRhsMap(k)) }
    (initMap, singleRhsMap)
  }

  /**
   * recursively replace all temp signals in RHS with known readable expression.
   * classify the map into two categories: readable (with the least temp signals) or not.
   * @param m
   * @param single
   * @param flops
   * @return the final mapping of assign statement
   */
  def refineMap(m: Map[String, String],
                single: Map[String, Boolean],
                flops: Set[String]):
  Map[String, String] = {

    def refineMapLoop(tbd: Map[String, String],
                      done: Map[String, String],
                      single: Map[String, Boolean],
                      flops: Set[String])
                     (i: Int):
    Map[String, String] = {
      if (false) { // used for debug
        println("refineMapLoop: " + i + " done.size=" + done.size + " tbd.size=" + tbd.size)
        done.foreach { case (k, v) => println(i + ": DONE : " + k + " = " + v) }
        tbd.foreach { case (k, v) => println(i + ": TBD  : " + k + " = " + v) }
      }

      // 1. split the tbd into "finished" (done) and "unfinished" (tbd)
      val (partDone, partTbd) = splitMap(tbd, flops)
      val newDone = done ++ partDone // the new dictionary

      // if (partDone.isEmpty || i >= 10) {
      if (partDone.isEmpty) {
        done ++ tbd
      } else {
        val newTbd = replaceExpr(partTbd, newDone, single)(flops)
        done ++ refineMapLoop(newTbd, newDone, single, flops)(i + 1)
      }
    }

    refineMapLoop(m, Map[String, String](), single: Map[String, Boolean], flops)(0)
  }

  /**
   * split the map into two: one is readable and one is not
   * @param m is the map to start with
   * @param flops
   * @return a map of readable assign statements and
   *         a map of assign statment to be working on later
   */
  private def splitMap(m: Map[String, String], flops: Set[String]):
  (Map[String, String], Map[String, String]) = {
    m.partition(t => exprIsFinal(t._2)(flops))
  }

  /** Replace the matched signal in RHS of each {LHS, RHS} in tbdMap with a meaningful name which
   * is obtained by lookup the doneMap
   *
   */
  private def replaceExpr(tbdMap: Map[String, String],
                          doneMap: Map[String, String],
                          singleMap: Map[String, Boolean])(
                           implicit flops: Set[String]):
  Map[String, String] = {
    //    for (t <- tbdMap) yield (t._1, replaceTemps(t._2, doneMap, singleMap))
    for ((k, v) <- tbdMap) yield (k, replaceTemps(v, doneMap, singleMap))
  }

  /**
   * substitute a temp signal with the mapping from the assign statement mapping, and
   * add parenthesis if being instructed.
   * @param s : the signal to be replaced
   * @param m : assign statement mapping
   * @param le : left parenthesis/curly bracket
   * @param re : right parenthsis/curly bracket
   * @return
   */
  private def substituteKeepWhiteSpace(s: String, m: Map[String, String])(le: String = "", re: String = ""): String = {
    val pat: Regex = ("""(\s*)""" + """(\S+)""" + """(\s*)""").r
    s match {
      case pat(lh, t, rh) => lh + le + m(t) + re + rh
      case _ => s
    }
  }

  /**
   * scan a string and replace all temp signals according to the assign statement mapping
   * @param s : the string to be replaced
   * @param am : assign statement map
   * @param sm : (RHS -> RHS is single) map.
   * @return
   */
  def replaceTemps(s: String,
                   am: Map[String, String],
                   sm: Map[String, Boolean]):
  String = {
    //println("replaceTemps...: " + s)

    def loop(list: List[String]): List[String] = list match {
      case Nil => Nil
      case x1 :: xs => {
        val k = x1.trim // the white space
        if (am.isDefinedAt(k) & isTempSignal(k)) {
          xs match {
            case "[" :: xt => {
              // SystemVerilog Feature not yet implemented. Select of concatenation
              // if (sm(k)) substituteWithWhiteSpace(x1, am)("", "") :: loop(xs)
              // else substituteWithWhiteSpace(x1, am)("{", "}") :: loop(xs) // vcs errors on this.
              x1 :: loop(xs)
            }
            case _ => {
              if (sm(k)) substituteKeepWhiteSpace(x1, am)("", "") :: loop(xs)
              else substituteKeepWhiteSpace(x1, am)("(", ")") :: loop(xs)
            }
          }
        } else {
          x1 :: loop(xs)
        }
      }
    }

    val list = parseVerilogExpr(s)
    loop(list).mkString("")
  }

}