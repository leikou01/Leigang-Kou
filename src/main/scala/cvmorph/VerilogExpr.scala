package cvmorph

import scala.util.matching.Regex

/**
 * basic utility functions to parse and analyze a verilog expression
 */
object VerilogExpr {

  def ops: List[String] = List(
    """\|+""", // | or ||
    """\&+""", // & or &&
    """\++""", // + or ++
    """\-+""", // - or --
    """\=+""", // = or ==
    """\!\=""", // !=
    """\<\=""", // <=
    """\>\=""", // >=
    """\<+""", // < or <<
    """\>+""", // > or >>
    """\~\&""", // ~&
    """\~\|""", // ~|
    """\~\^""", // ~^
    """\^\~""", // ^~
    """\!""", """\~""",
    """\^""",
    """\*""", """\/""", """\%""",
    """\?""", """\:"""
  )

  def delimiters: List[String] = List(
    """\[""", """\]""",
    """\{""", """\}""",
    """\(""", """\)""",
    """\,"""
  )

  def specials: List[String] = ops ::: delimiters


  /** parse a verilog expression into a list of ops, signals, keywords.
   * This is not a real verilog parser and thus may need ad-hoc hack later on
   * One unintuitive thing is that the parser keeps the whitespace surrounding around the keyword/signals
   *
   * @param s : a verilog expression
   * @return a list of ops, signals with surrounding whitespace, and keywords with surrounding white spaces
   */
  def parseVerilogExpr(s: String): List[String] = {
    // a regular expression being used for split the verilog expression into (lhs, op, rhs) format
    // if match, op will never be null, but lhs or rhs could be null
    val opsRegPattern: Regex = {
      val expr = """(.*)"""
      (expr + """(\s*""" + specials.mkString("|") + """\s*)""" + expr).r
    }

    def loop(s: String): List[String] = s match {
      case opsRegPattern(l, op, r) => {
        assert(op != null)
        loop(l) ::: op :: loop(r)
      }
      case _ => List(s)
    }

    val list = loop(s).
      filterNot(_.isEmpty) // filter out empty rhs and lhs
    assert(list.mkString("") == s) // because all white space are kept.
    list
  }

  /**
   * Whether or not the RHS of the original assign statement needs parenthesis and curley bracket
   * when the LHS is being replaced by RHS in other statements.
   *
   * Note that this is a quick and dirty way which relies on the format of current Chisel Verilog.
   * It can break at anytime when chisel/firrtl changes the format!!!
   *
   * @param s : a given verilog expression
   * @return : the expression is single, which means it does not need any parenthesis when being referenced
   */
  def chiselExprIsSingle(s: String): Boolean = {
    (s.split("""\s""")).length == 1
  }

  /**
   * extract the leading white space of a string
   *
   * @param s
   * @return
   */
  def extractIndent(s: String): Int = {
    val p = """(\s*).*""".r
    s match {
      case p(w) => w.length
      case _ => 0
    }
  }


  /**
   * Note that this function is flawed since
   * 1) it relies on the assumption " a <= b" is a non-blocking * statement. it can be fooled by something like
   *     assign x =
   *     foo <= bar;
   * it will give false positive
   * 2) it can also give false negative.
   *     always @(posedge clock) begin
   *         foo_Q[7 : 0] <= foo_D[7 : 0];
   *     end
   *
   * @param s
   * @return the name of the flop if the expr is the non-blocking statement
   */
  def extractFlopFromLine(s: String): Option[String] = {
    val nonBlockingExpr = """\s*(\S+)\s*<=\s*(\S+)\s*;.*""".r
    s match {
      case nonBlockingExpr(q, d) => Some(q)
      case _ => None
    }
  }

  def extractAssignFromLine(s: String): Option[(String, String)] = {
    //
    // +-------------------+-----------------+------------------------------+
    // | Greedy quantifier | Lazy quantifier |        Description           |
    // +-------------------+-----------------+------------------------------+
    // | *                 | *?              | Star Quantifier: 0 or more   |
    // | +                 | +?              | Plus Quantifier: 1 or more   |
    // | ?                 | ??              | Optional Quantifier: 0 or 1  |
    // | {n}               | {n}?            | Quantifier: exactly n        |
    // | {n,}              | {n,}?           | Quantifier: n or more        |
    // | {n,m}             | {n,m}?          | Quantifier: between n and m  |
    // +-------------------+-----------------+------------------------------+
    //   assign LH               =  RH                                               ;  // COMMENT
    //   assign s1_req_prio_0    =  _T ? io_req_bits_prio_0 : s1_req_reg_prio_0      ;  // @[SourceD.scala 93:19]
    // __......_xxxxxxxxxxxxx____._.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx______.__xxxxxxxxxxxxxxxxxxxxxxxxxx
    // 1  2    3 4           5   6 7  8                                        9     a b  c
    //
    //                   1 : zero or more whitespace, greedy
    //                       2: contains "assign" keyword
    //                             3: have at least 1 whitespace on the right side of "assign" keyword
    //                                   4: lhs: signal name, must be continuous nonwhite/non operator
    //                                      this implementation is flawed
    //                                       5: zero or more whitespace before "=" sign
    //                                            7: zero or more whitespace before the expression
    //                                                 8: rhs: everything except those whitespaces before ";"
    //                                                    non-greedy match here so that 9 can absorb all whitespaces
    //                   1         3         5    7        9    a
    //                   ____******____      ____*____     ____*____
    val assignPattern = "\\s*assign\\s+(\\S*)\\s*=\\s*(.*?)\\s*;\\s*(.*)$".r
    //                       assign     lhs      =     rhs     ;               // anchors
    //                                 xxxxxx signal name (does not comprehend assign {foo, bar} = bus[1:0])
    //                             ____ must be one or more space after "assign" keyword
    //                   ____ skip all whitespace (greedy match)
    //
    s match {
      case assignPattern(lhs, rhs, _) => Some(lhs, rhs)
      case _ => None
    }
  }

  def isAssignStatement(s: String): Boolean = extractAssignFromLine(s).nonEmpty

  def extractNonBlockingLine(s: String): Option[(String, String)] = {
    //                           _________ <=    ___    ;
    val nonBlockingRegExpr = """(\s*\S+\s*)<=(\s*\S+\s*);.*""".r
    s match {
      case nonBlockingRegExpr(lh, e) => Some(lh, e)
      case _ => None
    }
  }

  def isNonBlockingLine(s: String): Boolean = extractNonBlockingLine(s).nonEmpty

  def extractIfLine(s: String): Option[(String, String, String)] = {
    val ifRegExpr = ("""(\s*if\s*\()""" + """\s*(\S.*)\s*""" + """(\).*)""").r
    //                   __________              ____               _
    //                     if (                                     )
    s match {
      case ifRegExpr(lh, e, rh) => Some(lh, e, rh)
      case _ => None
    }
  }

  def isIfLine(s: String): Boolean = extractIfLine(s).nonEmpty

  def isTempSignal(s: String): Boolean = {
    s.trim matches """_\S+"""
  }

  def exprIsFinal(s: String)(implicit flops: Set[String] = Set()): Boolean = {
    val tl = parseVerilogExpr(s.trim)

    def loop(list: List[String]): List[String] = list match {
      case Nil => List()
      case x :: xs => {
        if (isTempSignal(x)) xs match {
          case "[" :: xt => loop(xs) // treat _T_1[7:0] as final since no replacement for this
          case _ => {
            if (flops.contains(x.trim)) loop(xs) // if any term of rhs is a flop,
            else x :: loop(xs)
          }
        }
        else loop(xs)
      }
    }

    loop(tl).isEmpty
  }

  //  def normalizeBracket(s: String): String = {
  //    val busPattern = """(.*?)(\s*\[\s*\d+\s*:\s*\d+\s*])(.*?)""".r
  //    //                   lh    e                         rh
  //    val bitPattern = """(.*?)(\s*\[\s*\d+\s*])(.*?)""".r
  //
  //    //                   lh    e                         rh    // divide into (right, middle, left) and
  //    // 1) process middle
  //    // 2) recursively working on left and right
  //    def loop(s: String): String = s match {
  //      case busPattern(lh, e, rh) => loop(lh) + e.replaceAll("\\s", "") + loop(rh)
  //      case bitPattern(lh, e, rh) => loop(lh) + e.replaceAll("\\s", "") + loop(rh)
  //      case _ => s
  //    }
  //
  //    loop(s)
  //  }

  //  /** If true, the RHS expression does need parentheses around it
  //   * when LHS is being replaced with RHS in other expressions.
  //   * Note it only works for the assign statement from the original Chisel Verilog.
  //   *
  //   * @param s : The RHS expression of the assign statement from the original Chisel Verilog
  //   * @return
  //   */
  //  def initRhsIsSingle(s: String): Boolean = {
  //    val list = parseVerilogExpr(s)
  //
  //    if (s.matches("\\(.*\\)")) true // already enclosed in a pair of ()
  //    else if (s.matches("\\{.*\\}")) true // already enclosed in a pair of {}
  //    else if (s.split("\\s+").toList.length <= 1) true //
  //    else false
  //  }


  //  def delimitWithWhiteSpace(s: String): String = parseVerilogExpr(s).mkString(" ")

  //  def finalizeAssignRhs(s: String): String = {
  //    def isSingle(s: String): Boolean = {
  //      normalizeExpr(s.trim).split("\\s+").toList.length <= 1
  //    }
  //
  //    val s1 = delimitWithWhiteSpace(s)
  //    if (s1.matches("\\(.*\\)")) s1 // already has parenthesis
  //    else if (s1.matches("\\{.*\\}")) s1 // already has parenthesis
  //    else if (isSingle(s1)) s1 // single signal
  //    else "( " + s1 + " )" // add parenthesis
  //  }

  //  def finalizeAssignRhsExpr(expr: String): String = {
  //    val list = parseVerilogExpr(expr)
  //
  //    def loop(list: List[String]): List[String] = {
  //      list match {
  //        case (sig :: "[" :: d1 :: ":" :: d2 :: "]" :: xs) => sig + "[" + d1 + ":" + d2 + "]" :: loop(xs)
  //        case _ => list
  //      }
  //    }
  //
  //    expr
  //  }

  //  def prettyInnerParenthesis(s: String): String = {
  //    val p = ("""(.*)""" + """\(\s*""" + """(.*?)""" + """\s*\)""" + """(.*)""").r;
  //    s match {
  //      case p(r, m, l) => prettyInnerParenthesis(r) + "(" + m + ")" + prettyInnerParenthesis(l)
  //      case _ => s
  //    }
  //  }

  //  def normalizeExpr(s: String): String = {
  //    val s1 = delimitWithWhiteSpace(s)
  //    val s2 = normalizeBracket(s1)
  //    prettyInnerParenthesis(s2)
  //  }

}