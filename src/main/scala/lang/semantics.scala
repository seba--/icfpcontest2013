package lang

import lang.Abstract._
import lang.Concrete._

object Semantics {
  
  case class UnboundVariable(x: Id) extends Exception
  case class UnexpectedOperator(expected: String, op: Operator) extends Exception
  case class EmptyBox() extends Exception
  
  type Value = Long
  type Var = (Id, Value)
  
  def try_eval(p: Prg)(v: Value): Option[Value] =
    try { Some(eval(p)(v)) } catch { case EmptyBox() => None }
  
  def eval(p: Prg)(v: Value): Value = 
    new Eval((p.x -> v)).eval(p.e)
  
  class Eval(in: Var) {
    private var foldNext: Option[Var] = None
    private var foldAcc: Option[Var] = None
    
    def eval(e: Exp): Value = e match {
      case b@Box() => if (b.isEmpty) throw new EmptyBox() else eval(b.e)
      case Zero() => 0L
      case One() => 1L
      case Var(x) =>
        if (x == in._1)
          in._2
        else if (foldNext.isDefined && x == foldNext.get._1)
          foldNext.get._2
        else if (foldAcc.isDefined && x == foldAcc.get._1)
          foldAcc.get._2
        else
          throw UnboundVariable(x)
    
      case IfZero(cond, yes, no) => {
        val cval = eval(cond)
        if (cval == 0)
          eval(yes)
        else
          eval(no)
    }
    
      case Fold(over, init, FoldFun(next, acc, body)) => {
        val v = eval(over)
        val b0 = (v & 0x00000000000000FFL)
        val b1 = (v & 0x000000000000FF00L) >> 8
        val b2 = (v & 0x0000000000FF0000L) >> 16
        val b3 = (v & 0x00000000FF000000L) >> 24
        val b4 = (v & 0x000000FF00000000L) >> 32
        val b5 = (v & 0x0000FF0000000000L) >> 40
        val b6 = (v & 0x00FF000000000000L) >> 48
        val b7 = (v & 0xFF00000000000000L) >> 56
        
        foldAcc = Some(acc -> eval(init))
        
        foldNext = Some(next -> b0)
        foldAcc = Some(acc -> eval(body))
        foldNext = Some(next -> b1)
        foldAcc = Some(acc -> eval(body))
        foldNext = Some(next -> b2)
        foldAcc = Some(acc -> eval(body))
        foldNext = Some(next -> b3)
        foldAcc = Some(acc -> eval(body))
        foldNext = Some(next -> b4)
        foldAcc = Some(acc -> eval(body))
        foldNext = Some(next -> b5)
        foldAcc = Some(acc -> eval(body))
        foldNext = Some(next -> b6)
        foldAcc = Some(acc -> eval(body))
        foldNext = Some(next -> b7)
        val result = eval(body)
        
        foldNext = None
        foldAcc = None
        
        result
      }
      
      case UApp(op, e) => {
        val v = eval(e)
        op match {
          case Operator.Not => ~v
          case Operator.Shl1 => v << 1
          case Operator.Shr1 => v >> 1
          case Operator.Shr4 => v >> 4
          case Operator.Shr16 => v >> 16
          case _ => throw UnexpectedOperator("unary", op)
        }
      }
      
      case BApp(op, e1, e2) => {
        val v1 = eval(e1)
        val v2 = eval(e2)
        op match {
          case Operator.And => v1 & v2
          case Operator.Or => v1 | v2
          case Operator.Xor => v1 ^ v2
          case Operator.Plus => v1 + v2
          case _ => throw UnexpectedOperator("binary", op)
        }
      }
    }
  }
  
}