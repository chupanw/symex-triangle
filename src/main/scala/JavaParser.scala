import java.io.FileReader

import Main.substitute

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


sealed trait SymbolicExpr {
  override def toString = pr(0)

  def pr(indent: Int): String

  def in(indent: Int): String = "    " * indent
}

case class SExpr(expr: String, arg: SymbolicExpr*) extends SymbolicExpr {
  override def pr(indent: Int): String =
    in(indent) + "-   " + expr + "\n" +
      arg.map(_.pr(indent + 1)).mkString
}

case class Literal(s: String) extends SymbolicExpr {
  override def pr(indent: Int): String = in(indent) + s + "\n"
}

object SymEx {
  val NONE = Literal("$NONE")
  val TRUE = Literal("$TRUE")

  def updateCtx(ctx: Map[String, SymbolicExpr], n: String, v: SymbolicExpr, pc: SymbolicExpr): Map[String, SymbolicExpr] =
    ctx + (n -> SExpr("ITE", pc, v, ctx.getOrElse(n, Literal("none"))))

  def andNot(a: SymbolicExpr, b: SymbolicExpr) = SExpr("&&", a, SExpr("!", b))

  def execute(p: List[Stmt], ctx: Map[String, SymbolicExpr], pc: SymbolicExpr): Map[String, SymbolicExpr] = p.headOption match {
    case None => ctx
    case Some(ReturnStmt(e)) => updateCtx(ctx, "$result", eval(e, ctx), pc)
    case Some(Assign(l, r)) => execute(p.tail, updateCtx(ctx, l, eval(r, ctx), pc), pc)
    case Some(IfStmt(c, t, e)) =>
      val cv = eval(c, ctx)
      val leftCtx = execute(List(t), ctx, SExpr("&&", pc, cv))
      val rightCtx = execute(e.toList, leftCtx, andNot(pc, cv))
      execute(p.tail, rightCtx, pc)
  }

  def eval(x: Expr, ctx: Map[String, SymbolicExpr]): SymbolicExpr = x match {
    case Id(x) =>
      if (x startsWith "_mut") Literal(x)
      else ctx(x)
    case Val(x) => Literal(x)
    case Call(n, a) => SExpr("call", Literal(n), Literal(a.toString()))
    case BOp(l, o, r) => SExpr(o, eval(l, ctx), eval(r, ctx))
    case ITE(i, t, e) =>
      val _i = eval(i, ctx)
      if (_i == TRUE) eval(t, ctx)
      else SExpr("?", _i, eval(t, ctx), eval(e, ctx))
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

  val astInlined = inlineFun(ast.get, funs.get)

  println(astInlined.map(Print.print).mkString("\n"))

  println(SymEx.execute(astInlined,
    Map("INVALID" -> Literal("0"), "ISOSCELES" -> Literal("1"), "SCALENE" -> Literal("2"), "EQUILATERAL" -> Literal("3"),
      "a" -> Literal("α"), "b" -> Literal("β"), "c" -> Literal("γ")),
    SymEx.TRUE))

}