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

  case class Prg(x: Id, e: Exp)
  
  abstract class Exp
  case class Zero() extends Exp
  case class One() extends Exp
  case class Var(x: Id) extends Exp
  case class IfZero(cond: Exp, yes: Exp, no: Exp) extends Exp
  case class Fold(over: Exp, init: Exp, f: FoldFun) extends Exp
  case class UApp(op: Unary, e: Exp) extends Exp
  case class BApp(op: Binary, e1: Exp, e2: Exp) extends Exp
  
  final case class FoldFun(next: Id, acc: Id, body: Exp)
  
}

object Concrete {
  
  import Abstract._
  
  type Error = String
  type Result[A] = Either[(A, String), Error]
  
  def parse(s: String): Result[Prg] = parsePrg(s)
  
  def parsePrg: String => Result[Prg] =
    inParens(s => {
      val Left((_, s1)) = word("lambda")(layout(s))
      val Left((x, s2)) = inParens(parseId)(layout(s1))
      val Left((e, s3)) = parseExp(layout(s2))
      Left((Prg(x, e), s3))
    })
      
  
  def parseId(s: String): Result[String] = {
    var length: Int = 0
    while (s.size > 0) {
      val c = s(0)
      if ((c >= 'a' && c <= 'z') || c == '_' || (c >= '0' && c <= '9'))
        length += 1
      else
        return if (length > 0) 
                 Left((s.substring(0, length), s.substring(length)))
               else
                 Right("Expected identifier")
    }
    return if (length > 0) 
             Left((s.substring(0, length), s.substring(length)))
           else
             Right("Expected identifier")
  }
  
  def parseExp(s: String): Result[Exp] = inParens(parseExp1)(s)
  
  def parseExp1(s: String): Result[Exp] =
    if (s.startsWith("if0")) {
      val Left((_, s1)) = word("if0")(layout(s))
      val Left((e1, s2)) = parseExp(layout(s1))
      val Left((e2, s3)) = parseExp(layout(s2))
      val Left((e3, s4)) = parseExp(layout(s3))
      Left((IfZero(e1, e2, e3), s4))
    }
    else if (s.startsWith("fold")) {
      val Left((_, s1)) = word("fold")(layout(s))
      val Left((e1, s2)) = parseExp(layout(s1))
      val Left((e2, s3)) = parseExp(layout(s2))
      val Left((f, s4)) = inParens(parseFoldFun)(layout(s3))
      Left((Fold(e1, e2, f), s4))
    }
    else {
      parseUnary(layout(s)) match {
        case Left((op, s2)) => {
          val Left((e, s3)) = parseExp(layout(s2))
          Left((UApp(op, e), s3))
        }
        case Right(_) =>
          parseBinary(layout(s)) match {
            case Left((op, s2)) => {
              val Left((e1, s3)) = parseExp(layout(s2))
              val Left((e2, s4)) = parseExp(layout(s3))
              Left((BApp(op, e1, e2), s3))
            }
            case Right(_) => Right("Expected expression")
          }
      }
    }

  def parseUnary(s: String): Result[Unary] = {
    val Left((op, s2)) = parseId(layout(s))
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
      Left((operator, s2))
    else
      Right("Expected unary operator")
  }

  def parseBinary(s: String): Result[Binary] = {
    val Left((op, s2)) = parseId(layout(s))
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
      Left((operator, s2))
    else
      Right("Expected unary operator")
  }

  def parseFoldFun(s: String): Result[FoldFun] = {
    val Left((_, s1)) = word("lambda")(layout(s))
    val Left(((x,y), s2)) = inParens({s =>
        val Left((x, s21)) = parseId(layout(s))
        val Left((y, s22)) = parseId(layout(s21))
        Left(((x, y), s22))
      })(layout(s1))
    val Left((e, s3)) = parseExp(layout(s2))
    Left((FoldFun(x, y, e), s3))
  }
    
  def layout(s: String): String = {
    var length: Int = 0
    while (s.size > 0) {
      val c = s(0)
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
        Right("Expected " + w)
      else
        Left(((), rest))
    }
    else
      Right("Expected " + w)
  
  def inParens[A](parser: String => Result[A])(s: String): Result[A] = {
    val size = s.size
    if (size >= 2 && s(0) == '(')
      parser(layout(s.substring(1, size - 1))) match {
      case Left((a, rest)) => {
        val r = layout(rest)
        if (r.size > 0 && r(0) == ')')
          return Left((a, r.substring(1)))
        else
          return Right("Expected closing parenthesis")  
      }
      case r@Right(_) => return r
    }
    return Right("Expected opening parenthesis")
  }
}

