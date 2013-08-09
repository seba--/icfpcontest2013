package lang

import scala.util.Either

object Abstract {
  
  type Id = String

  object Unary extends Enumeration {
    val Not, Shl1, Shr1, Shr4, Shr16 = Value
  }
  import Unary._
  type Unary = Unary.Value
  
  object Binary extends Enumeration {
    val And, Or, Xor, Plus = Value
  }
  import Binary._
  type Binary = Binary.Value

  case class Prg(x: Id, e: Exp) { 
    override def toString = "(lambda (" + x + ") " + e + ")" 
  }
  
  abstract class Exp
  case class Zero() extends Exp { override def toString = "0" }
  case class One() extends Exp { override def toString = "1" }
  case class Var(x: Id) extends Exp { override def toString = x }
  case class IfZero(cond: Exp, yes: Exp, no: Exp) extends Exp { override def toString = "(if0 " + cond + " " + yes + " " + no + ")" }
  case class Fold(over: Exp, init: Exp, f: FoldFun) extends Exp { override def toString = "(fold " + over+ " " + init + " " + f + ")" }
  case class UApp(op: Unary, e: Exp) extends Exp { override def toString = "(" + op.toString.toLowerCase + " " + e + ")" }
  case class BApp(op: Binary, e1: Exp, e2: Exp) extends Exp { override def toString = "(" + op.toString.toLowerCase + " " + e1 + " " + e2 + ")" }
  
  final case class FoldFun(next: Id, acc: Id, body: Exp) { 
    override def toString = "(lambda (" + next + " " + acc + ") " + body + ")" 
  }
  
}

object Concrete {
  
  import Abstract._
  
  case class ParseException(msg: String, rest: String) extends Exception
  
  type Result[A] = (A, String)
  
  def parse(s: String): Result[Prg] = parsePrg(s)
  
  def parsePrg: String => Result[Prg] =
    inParens(s => {
      val (_, s1) = word("lambda")(layout(s))
      val (x, s2) = inParens(parseId)(layout(s1))
      val (e, s3) = parseExp(layout(s2))
      (Prg(x, e), s3)
    })
      
  
  def parseId(s: String): Result[String] = {
    var length: Int = 0
    while (length < s.size) {
      val c = s(length)
      if ((c >= 'a' && c <= 'z') || c == '_' || (c >= '0' && c <= '9'))
        length += 1
      else
        return if (length > 0) 
                 (s.substring(0, length), s.substring(length))
               else
                 throw ParseException("Expected identifier", s)
    }
    return if (length > 0) 
             (s.substring(0, length), s.substring(length))
           else
             throw ParseException("Expected identifier", s)
  }
  
  def parseExp(s: String): Result[Exp] =
    if (s.startsWith("0"))
      (Zero(), s.substring(1))
    else if (s.startsWith("1"))
      (One(), s.substring(1))
    else if (s.size > 0 && (s(0) >= 'a' && s(0) <= 'z')) {
      val (x, s2) = parseId(s)
      (Var(x), s2)
    }
    else
      inParens(parseExp1)(s)
  
  def parseExp1(s: String): Result[Exp] =
    if (s.startsWith("if0")) {
      val (_, s1) = word("if0")(layout(s))
      val (e1, s2) = parseExp(layout(s1))
      val (e2, s3) = parseExp(layout(s2))
      val (e3, s4) = parseExp(layout(s3))
      (IfZero(e1, e2, e3), s4)
    }
    else if (s.startsWith("fold")) {
      val (_, s1) = word("fold")(layout(s))
      val (e1, s2) = parseExp(layout(s1))
      val (e2, s3) = parseExp(layout(s2))
      val (f, s4) = inParens(parseFoldFun)(layout(s3))
      (Fold(e1, e2, f), s4)
    }
    else {
      tryParseUnary(layout(s)) match {
        case Some((op, s2)) => {
          val (e, s3) = parseExp(layout(s2))
          (UApp(op, e), s3)
        }
        case None =>
          tryParseBinary(layout(s)) match {
            case Some((op, s2)) => {
              val (e1, s3) = parseExp(layout(s2))
              val (e2, s4) = parseExp(layout(s3))
              (BApp(op, e1, e2), s4)
            }
            case None => throw ParseException("Expected expression", s)
          }
      }
    }

  def tryParseUnary(s: String): Option[Result[Unary]] = {
    val (op, s2) = parseId(layout(s))
    val operator = 
    if (op == "not")
      Unary.Not
    else if (op == "shl1")
      Unary.Shl1 
    else if(op == "shr1")
      Unary.Shr1
    else if (op == "shr4")
      Unary.Shr4
    else if (op == "shr16")
      Unary.Shr16
    else 
      null
    if (operator != null)
      Some((operator, s2))
    else
      None
  }

  def tryParseBinary(s: String): Option[Result[Binary]] = {
    val (op, s2) = parseId(layout(s))
    val operator = 
    if (op == "and")
      Binary.And
    else if (op == "or")
      Binary.Or 
    else if(op == "xor")
      Binary.Xor
    else if (op == "plus")
      Binary.Plus
    else 
      null
    if (operator != null)
      Some((operator, s2))
    else
      None
  }

  def parseFoldFun(s: String): Result[FoldFun] = {
    val (_, s1) = word("lambda")(layout(s))
    val ((x,y), s2) = inParens({s =>
        val (x, s21) = parseId(layout(s))
        val (y, s22) = parseId(layout(s21))
        ((x, y), s22)
      })(layout(s1))
    val (e, s3) = parseExp(layout(s2))
    (FoldFun(x, y, e), s3)
  }
    
  def layout(s: String): String = {
    var length: Int = 0
    while (length < s.size) {
      val c = s(length)
      if (c == ' ' || c == '\n' || c == '\r' || c == '\t')
        length += 1
      else
        return s.substring(length)
    }
    return s.substring(length)
  } 
  
  def word(w: String)(s: String): Result[Unit] = 
    if (s.startsWith(w)) {
      val rest = s.substring(w.size)
      if (rest.size > 0 && ((rest(0) >= 'a' && rest(0) <= 'z') || rest(0) == '_' || (rest(0) >= '0' && rest(0) <= '9')))
        throw ParseException("Expected " + w, s)
      else
        ((), rest)
    }
    else
      throw ParseException("Expected " + w, s)
  
  def inParens[A](parser: String => Result[A])(s: String): Result[A] = {
    val size = s.size
    if (size >= 2 && s(0) == '(') {
      val (a, rest) = parser(layout(s.substring(1)))
      val r = layout(rest)
      if (r.size > 0 && r(0) == ')')
        return (a, r.substring(1))
      else
        throw ParseException("Expected closing parenthesis", r)  
    }
    throw ParseException("Expected opening parenthesis", s)
  }
}

