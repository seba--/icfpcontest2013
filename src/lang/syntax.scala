
trait Abstract {
  
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
  case class Zero extends Exp
  case class One extends Exp
  case class Var(x: Id) extends Exp
  case class IfZero(cond: Exp, yes: Exp, no: Exp) extends Exp
  case class Fold(over: Exp, init: Exp, f: FoldFun) extends Exp
  case class UApp(op: Unary, e: Exp) extends Exp
  case class BApp(op: Binary, e1: Exp, e2: Exp) extends Exp
  
  final case class FoldFun(next: Id, acc: Id, body: Exp)
  
}

