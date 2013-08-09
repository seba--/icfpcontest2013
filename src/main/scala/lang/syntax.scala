
import scala.util.Either

object Abstract {
  
  type Id = String

  object Unary extends Enumeration {
    type Unary = Value
    val Not, Shl1, Shr1, Shr4, Shr16 = Value
  }
  import Unary._
  
  object Binary extends Enumeration {
    type Binary = Value
    val And, Or, Xor, Plus = Value
  }
  import Binary._

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
  
  def parse(s: String) = parsePrg(s)
  
  def parsePrg : String => Result[Prg] =
    inParens(seq(word("lambda"), inParens(parseId), parseExp))
  
  def layout(str: String): Result[Unit] = {
    var s: String = str
    while (s.size > 0) {
      val c = s(0)
      if (c == ' ' || c == '\n' || c == '\r' || c == '\t')
        s = s.substring(1)
      else
        return Left(((), s))
    }
    return Left(((), s))
  } 
  
  def word(w: String)(s: String): Result[Unit] = 
    if (s.startsWith(w))
      Left(((), s.substring(w.size)))
    else
      Right("expected " + w)
  
  def inParens[A](parser: String => Result[A])(s: String): Result[A] = {
    val size = s.size
    if (size >= 2 && s(0) == '(' && s(size - 1) == ')')
      return parser(s.substring(1, size - 1))
    return Right("Expected parenthesized form")
  }
}

