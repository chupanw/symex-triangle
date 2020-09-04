import java.io.FileReader
import java.util

import SymEx.Z3Expr
import com.microsoft.z3.{ArithExpr, BoolExpr, Context, IntExpr, IntNum, Model, Solver, Status}

import scala.util.parsing.combinator.RegexParsers

object JavaParser extends RegexParsers {
  override def skipWhitespace = true

  //  override val whiteSpace = "[ \t\r\f]+".r

  def identifier: Parser[String] =
    "[a-zA-Z_][a-zA-Z0-9_]*".r

  def value: Parser[String] =
    "[0-9]+".r

  def op: Parser[String] =
    "[<>=%&\\|/*+\\-!]+".r

  def prog: Parser[List[Stmt]] = rep(stmt)

  def stmt: Parser[Stmt] = returnStmt | ifStmt | assign

  def returnStmt: Parser[ReturnStmt] = "return" ~> expr <~ ";" ^^ ReturnStmt.apply

  def ifStmt: Parser[IfStmt] = ("if" ~ "(" ~> expr <~ ")") ~ stmt ~ opt("else" ~> stmt) ^^ { case c ~ t ~ e => IfStmt(c, t, e) }

  def expr: Parser[Expr] =
    ite |
      bop |
      call | identifier ^^ Id.apply | value ^^ Val.apply

  def call: Parser[Call] = (identifier <~ "(") ~ repsep(expr, ",") <~ ")" ^^ { case i ~ p => Call(i, p) }

  def assign: Parser[Assign] = (identifier <~ "=") ~ expr <~ ";" ^^ { case i ~ e => Assign(i, e) }

  def ite: Parser[ITE] = "(" ~> expr ~ "?" ~ expr ~ ":" ~ expr <~ ")" ^^ { case i ~ _ ~ t ~ _ ~ e => ITE(i, t, e) }

  def bop: Parser[BOp] = "(" ~> expr ~ op ~ expr <~ ")" ^^ { case l ~ o ~ r => BOp(l, o, r) }

  def funs = rep(fun)

  def fun: Parser[Fun] = ("public" ~ "static" ~ ("boolean" | "int") ~> identifier <~ ("(" ~ ("boolean" | "int") ~ "left" ~ "," ~ ("boolean" | "int") ~ "right" ~ "," ~ "boolean" ~ "..." ~ "mutants" ~ ")" ~ "{")) ~ returnStmt <~ "}" ^^ { case n ~ b => Fun(n, b) }
}

case class Fun(name: String, body: ReturnStmt)

sealed trait Stmt

case class ReturnStmt(x: Expr) extends Stmt

case class Assign(id: String, expr: Expr) extends Stmt

case class IfStmt(cond: Expr, thenS: Stmt, elseS: Option[Stmt]) extends Stmt

sealed trait Expr

case class Id(name: String) extends Expr

case class Val(v: String) extends Expr

case class Call(name: String, args: List[Expr]) extends Expr

case class ITE(i: Expr, t: Expr, e: Expr) extends Expr

case class BOp(l: Expr, o: String, r: Expr) extends Expr

object SymEx {
  type Z3Expr = com.microsoft.z3.Expr
  val cfg = new util.HashMap[String, String]()
  cfg.put("model", "true")
  val ctx = new Context(cfg)
  def getSolver: Solver = ctx.mkSolver()
  val NONE: IntNum = ctx.mkInt(-1)
  val TRUE: BoolExpr = ctx.mkTrue()
  val FALSE: BoolExpr = ctx.mkFalse()

  def updateCtx(values: Map[String, Z3Expr], n: String, v: Z3Expr, pc: BoolExpr): Map[String, Z3Expr] = {
    val ite = ctx.mkITE(pc, v, values.getOrElse(n, NONE))
    values + (n -> ite)
  }

  def andNot(a: BoolExpr, b: BoolExpr): BoolExpr = {
    ctx.mkAnd(a, ctx.mkNot(b))
  }

  var globalPC: BoolExpr = ctx.mkTrue()
  def reset(): Unit = globalPC = ctx.mkTrue()

  def execute(p: List[Stmt], values: Map[String, Z3Expr], pc: BoolExpr): Map[String, Z3Expr] = p.headOption match {
    case None => values
    case Some(ReturnStmt(e)) =>
      globalPC = andNot(globalPC, pc)
      updateCtx(values, "$result", eval(e, values), pc)
    case Some(Assign(l, r)) =>
      execute(p.tail, updateCtx(values, l, eval(r, values), pc), pc)
    case Some(IfStmt(c, t, e)) =>
      val cv = eval(c, values)
      val leftValues = execute(List(t), values, ctx.mkAnd(pc, globalPC, cv.asInstanceOf[BoolExpr]))
      val rightValues = execute(e.toList, leftValues, andNot(ctx.mkAnd(pc, globalPC), cv.asInstanceOf[BoolExpr]))
      execute(p.tail, rightValues, ctx.mkAnd(pc, globalPC))
  }

  def eval(x: Expr, values: Map[String, Z3Expr]): Z3Expr = x match {
    case Id(x) =>
      if ((x startsWith "_mut") && !values.contains(x))
        ctx.mkFalse()
      else values(x)
    case Val(x) => ctx.mkInt(x)
    case Call(n, a) =>
      throw new RuntimeException("Calls should be inlined already")
    case BOp(l, o, r) =>
      val ll = eval(l, values)
      val rr = eval(r, values)
      (ll, rr) match {
        case (lll: IntExpr, rrr: IntExpr) =>
          o match {
            case ">" => ctx.mkGt(lll, rrr)
            case ">=" => ctx.mkGe(lll, rrr)
            case "<" => ctx.mkLt(lll, rrr)
            case "<=" => ctx.mkLe(lll, rrr)
            case "==" => ctx.mkEq(lll, rrr)
            case "!=" => ctx.mkNot(ctx.mkEq(lll, rrr))
            case "+" => ctx.mkAdd(lll, rrr)
            case "%" => ctx.mkMod(lll, rrr)
            case "-" => ctx.mkSub(lll, rrr)
            case "*" => ctx.mkMul(lll, rrr)
            case "/" => ctx.mkDiv(lll, rrr)
            case _ => throw new RuntimeException("Please implement " + o)
          }
        case (lll: BoolExpr, rrr: BoolExpr) =>
          o match {
            case "||" => ctx.mkOr(lll, rrr)
            case "&&" => ctx.mkAnd(lll, rrr)
            case _ => throw new RuntimeException("Please implement " + o)
          }
      }
    case ITE(i, t, e) =>
      val _i = eval(i, values).asInstanceOf[BoolExpr]
      if (_i == TRUE) eval(t, values)
      else if (_i == FALSE) eval(e, values)
      else ctx.mkITE(_i, eval(t, values), eval(e, values))
  }

}

object Print {
  def print(s: Stmt): String = s match {
    case ReturnStmt(e) => s"return ${print(e)};\n"
    case IfStmt(i, t, e) => s"if (${print(i)}) ${print(t)}" + e.map(print).map(" else " + _).getOrElse("")
    case Assign(l, r) => l + "=" + print(r)
  }

  def print(e: Expr): String = e match {
    case i: Id => i.name
    case i: Val => i.v
    case ITE(i, t, e) => s"(${print(i)} ? ${print(t)} : ${print(e)})"
    case BOp(l, o, r) => s"(${print(l)} $o ${print(r)})"
    case Call(name, args) => name + "(" + args.map(print).mkString(", ") + ")"
  }
}

object Main extends App {

  assert(JavaParser.parseAll(JavaParser.stmt, "return a;").successful)
  assert(JavaParser.parseAll(JavaParser.ifStmt, "if (a) return b;").successful)
  assert(JavaParser.parseAll(JavaParser.call, "a(b,c,e)").successful)
  assert(JavaParser.parseAll(JavaParser.call, "a(b,c(f,g),e)").successful)
  println(JavaParser.parseAll(JavaParser.ifStmt, "if (a) return b; else return c;"))
  println(JavaParser.parseAll(JavaParser.bop, "(a!=B)"))
  println(JavaParser.parseAll(JavaParser.returnStmt, "return (a!=B);"))


  val ast = JavaParser.parseAll(JavaParser.prog, new FileReader("triangle.txt"))
  println(ast)

  val funs = JavaParser.parseAll(JavaParser.funs, new FileReader("fun.txt"))
  println(funs)

  def substitute(e: Expr, from: String, to: Expr): Expr = {
    def r(e: Expr): Expr = e match {
      case Id(n) => if (n == from) to else Id(n)
      case i: Val => i
      case ITE(i, t, e) => ITE(r(i), r(t), r(e))
      case BOp(l, o, rr) => BOp(r(l), o, r(rr))
      case Call(name, args) => Call(name, args.map(r))
    }

    r(e)
  }

  def inlineFun(ast: List[Stmt], funs: List[Fun]): List[Stmt] = {
    def inlineS(s: Stmt): Stmt = s match {
      case ReturnStmt(e) => ReturnStmt(inlineE(e))
      case IfStmt(i, t, e) => IfStmt(inlineE(i), inlineS(t), e.map(inlineS))
      case Assign(l, r) => Assign(l, inlineE(r))
    }

    def inlineE(e: Expr): Expr = e match {
      case i: Id => i
      case i: Val => i
      case ITE(i, t, e) => ITE(inlineE(i), inlineE(t), inlineE(e))
      case BOp(l, o, r) => BOp(inlineE(l), o, inlineE(r))
      case Call(name, args) =>
        val fun = funs.find(_.name == name)
        assert(fun.isDefined, s"function $name not found")
        var expr = fun.get.body.x
        expr = substitute(expr, "left", args(0))
        expr = substitute(expr, "right", args(1))
        expr = substitute(expr, "m0", args(2))
        if (args.length > 3)
          expr = substitute(expr, "m1", args(3))
        if (args.length > 4)
          expr = substitute(expr, "m2", args(4))
        if (args.length > 5)
          expr = substitute(expr, "m3", args(5))
        if (args.length > 6)
          expr = substitute(expr, "m4", args(6))
        inlineE(expr)
    }

    ast.map(inlineS)
  }

  def genBaseMap(): Map[String, Z3Expr] = {
    Map(
      "INVALID" -> SymEx.ctx.mkInt(0),
      "ISOSCELES" -> SymEx.ctx.mkInt(1),
      "SCALENE" -> SymEx.ctx.mkInt(2),
      "EQUILATERAL" -> SymEx.ctx.mkInt(3),
      "a" -> SymEx.ctx.mkIntConst("a"),
      "b" -> SymEx.ctx.mkIntConst("b"),
      "c" -> SymEx.ctx.mkIntConst("c")
    )
  }

  def genMutMap(enabled: String*): Map[String, Z3Expr] = {
    enabled.map(x => x -> SymEx.ctx.mkTrue()).toMap
  }

  val astInlined = inlineFun(ast.get, funs.get)

  /**
   * Verify whether an SSHOM is a strict-SSHOM. If a solution can be found, it is a strict-SSHOM.
   * @param enabled constituent FOMs which together form a HOM
   * @param solutions a set of strict-SSHOMs as result
   */
  def testStrictSSHOM(enabled: List[String], solutions: collection.mutable.ListBuffer[List[String]]): Unit = {
    val solver = SymEx.getSolver
    val baseMap = genBaseMap()

    SymEx.reset()
    val baselineValues = SymEx.execute(astInlined, baseMap, SymEx.TRUE)
    val baselineResult = baselineValues("$result")

    val fomResults = enabled.map(x => {
      SymEx.reset()
      val fomValues = SymEx.execute(astInlined, baseMap ++ genMutMap(x), SymEx.TRUE)
      fomValues("$result")
    })

    val fomConstrains = fomResults.map(r => SymEx.ctx.mkNot(SymEx.ctx.mkEq(baselineResult, r)))
    solver.add(SymEx.ctx.mkAnd(fomConstrains:_*))

    SymEx.reset()
    val homValues = SymEx.execute(astInlined, baseMap ++ genMutMap(enabled:_*), SymEx.TRUE)
    val homResult = homValues("$result")

    solver.add(SymEx.ctx.mkEq(baselineResult, homResult))

    var model: Model = null
    if (solver.check() == Status.SATISFIABLE) {
      model = solver.getModel
      print(s"Solution for SSHOM $enabled: ")
      print(s"a = ${model.evaluate(baselineValues("a"), false)}, ")
      print(s"b = ${model.evaluate(baselineValues("b"), false)}, ")
      println(s"c = ${model.evaluate(baselineValues("c"), false)}")
      solutions.addOne(enabled.toList)
    } else {
      println(s"SSHOM $enabled unsatisfiable")
    }
  }

  /**
   * Verify whether a HOM is an SSHOM. If a solution cannot be found, it is an SSHOM.
   * @param enabled constituent FOMs which together form a HOM
   * @param solutions a set of SSHOMs as result
   */
  def testSSHOM(enabled: List[String], solutions: collection.mutable.ListBuffer[List[String]]): Unit = {
    val solver = SymEx.getSolver
    val baseMap = genBaseMap()

    SymEx.reset()
    val baselineValues = SymEx.execute(astInlined, baseMap, SymEx.TRUE)
    val baselineResult = baselineValues("$result")

    val fomResults = enabled.map(x => {
      SymEx.reset()
      val fomValues = SymEx.execute(astInlined, baseMap ++ genMutMap(x), SymEx.TRUE)
      fomValues("$result")
    })

    val fomConstrains = fomResults.map(r => SymEx.ctx.mkEq(baselineResult, r))
    solver.add(SymEx.ctx.mkOr(fomConstrains:_*))

    SymEx.reset()
    val homValues = SymEx.execute(astInlined, baseMap ++ genMutMap(enabled:_*), SymEx.TRUE)
    val homResult = homValues("$result")

    solver.add(SymEx.ctx.mkNot(SymEx.ctx.mkEq(baselineResult, homResult)))

    var model: Model = null
    if (solver.check() == Status.SATISFIABLE) {
      model = solver.getModel
      print(s"Solution for SSHOM $enabled: ")
      print(s"a = ${model.evaluate(baselineValues("a"), false)}, ")
      print(s"b = ${model.evaluate(baselineValues("b"), false)}, ")
      println(s"c = ${model.evaluate(baselineValues("c"), false)}")
    } else {
      println(s"SSHOM $enabled unsatisfiable")
      solutions.addOne(enabled.toList)
    }
  }

  def testNonEquivalent(enabled: List[String], solutions: collection.mutable.ListBuffer[List[String]]): Unit = {
    val solver = SymEx.getSolver
    val baseMap = genBaseMap()

    SymEx.reset()
    val baselineValues = SymEx.execute(astInlined, baseMap, SymEx.TRUE)
    val baselineResult = baselineValues("$result")

    SymEx.reset()
    val homValues = SymEx.execute(astInlined, baseMap ++ genMutMap(enabled:_*), SymEx.TRUE)
    val homResult = homValues("$result")

    solver.add(SymEx.ctx.mkNot(SymEx.ctx.mkEq(baselineResult, homResult)))

    if (solver.check() == Status.SATISFIABLE) {
      solutions.addOne(enabled)
    }
  }

  def verifyVarexSSHOMs(): Unit = {
    val solutions = new collection.mutable.ListBuffer[List[String]]()
    val lines = io.Source.fromFile("sshom.txt").getLines()
    lines foreach {l => {
      val enabled = l.tail.init.split(",").map(_.trim)
      // these SSHOMs are guaranteed to be non-equivalent already
      testSSHOM(enabled.toList, solutions)
    }}
    println(solutions.map(_.mkString(", ")).mkString("\n"))
    println(s"# Solutions: ${solutions.size}")
  }

  def verifyVarexStrictSSHOMs(): Unit = {
    val solutions = new collection.mutable.ListBuffer[List[String]]()
    val lines = io.Source.fromFile("ideal-sshom-bf-2.txt").getLines()
    lines foreach {l => {
      val enabled = l.split(",").map(_.trim)
      testStrictSSHOM(enabled.toList, solutions)
    }}
    println(solutions.map(_.mkString(", ")).mkString("\n"))
    println(s"# Solutions: ${solutions.size}")
  }

  def bruteForceDegree(degree: Int): Unit = {
    val nonEquivalenceSolutions = new collection.mutable.ListBuffer[List[String]]()
    val sshomSolutions = new collection.mutable.ListBuffer[List[String]]()
    val foms = (0 to 127).toList map {i => s"_mut$i"}
    def gen(l: List[String], d: Int): List[List[String]] = {
      if (d == l.size) {
        List(l)
      }
      else if (d == 1) {
        l.map(x => List(x))
      }
      else {
        val head = l.head
        val includeHead = gen(l.tail, d - 1).map(x => head :: x)
        val excludeHead = gen(l.tail, d)
        includeHead ::: excludeHead
      }
    }
    val groups = io.Source.fromFile("triangle-mutant-group.txt").getLines().toList.map(_.split(" ").map(_.trim).toSet)
    val combinations = gen(foms, degree).filterNot(c => groups.exists(g => c.count(m => g.contains(m)) >= 2))
    println(s"Trying ${combinations.size} combinations...")
    combinations foreach {enabled => {
      testNonEquivalent(enabled, nonEquivalenceSolutions)
      testSSHOM(enabled, sshomSolutions)
    }}
    val overallSolutions = nonEquivalenceSolutions intersect sshomSolutions
    println(overallSolutions.map(_.mkString(", ")).mkString("\n"))
    println(s"# Solutions: ${overallSolutions.size}")
  }

    verifyVarexSSHOMs()
//  bruteForceDegree(2)
//  verifyVarexStrictSSHOMs()
}

object Motivation extends App {
  val cfg = new util.HashMap[String, String]()
  cfg.put("model", "true")
  val ctx = new Context(cfg)
  val solver: Solver = ctx.mkSolver()

  val a = ctx.mkIntConst("a")
  val b = ctx.mkIntConst("b")
  // baseline
  val aEqualOne = ctx.mkEq(a, ctx.mkInt(1))
  val aLtb = ctx.mkLt(a, b)
  val aGtb = ctx.mkGt(a, b)
  val baseline = ctx.mkITE(aEqualOne, aLtb, aGtb)
  println(s"baseline: $baseline")

  val m1 = ctx.mkITE(ctx.mkNot(aEqualOne), aLtb, aGtb)
  println(s"m1: $m1")

  val m2 = ctx.mkITE(aEqualOne, ctx.mkGe(a, b), aGtb)
  println(s"m2: $m2")

  val m1_m2 = ctx.mkITE(ctx.mkNot(aEqualOne), ctx.mkGe(a, b), aGtb)
  println(s"m1_m2: $m1_m2")

  solver.add(ctx.mkNot(ctx.mkEq(m1_m2, baseline)))
  solver.add(ctx.mkOr(ctx.mkEq(m1, baseline), ctx.mkEq(m2, baseline)))

  var model: Model = null
  if (solver.check() == Status.SATISFIABLE) {
    model = solver.getModel
    println(s"a = ${model.evaluate(a, false)}")
    println(s"b = ${model.evaluate(b, false)}")
  } else {
    println("Unsatisfiable")
  }
}

object CountDegree extends App {
  val lines = io.Source.fromFile("sshom.txt").getLines().toList
  val splitLines = lines.map(l => l.split(","))
  2 to 10 foreach {i => {
    println(s"Degree $i: ${splitLines.count(l => l.size == i)}")
  }}
}

object Subsume extends App {
  val varex = io.Source.fromFile("ideal-sshom.txt").getLines().toList.map(x => x.split(",").map(_.trim).toSet).filter(_.size == 3)
  val bf3 = io.Source.fromFile("ideal-sshom-bf-3.txt").getLines().toList.map(x => x.split(",").map(_.trim).toSet).toSet
  for (s <- varex) {
    if (!bf3.contains(s)) {
      println(s)
    }
  }
}
