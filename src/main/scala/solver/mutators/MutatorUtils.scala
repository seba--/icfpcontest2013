package solver.mutators

import lang.Abstract._
import lang.Abstract.Operator._

object MutatorUtils {
  def getMinimalExpressionForOperator(op:Operator) : Exp = {
    op match {
      case Not => UApp(Not, Box())
      case Shl1 => UApp(Shl1, Box())
      case Shr1 => UApp(Shr1, Box())
      case Shr4 => UApp(Shr4, Box())
      case Shr16 => UApp(Shr16, Box())
      case And => BApp(And, Box(), Box())
      case Or => BApp(Or, Box(), Box())
      case Plus => BApp(Plus, Box(), Box())
      case Xor => BApp(Xor, Box(), Box())
      case If0 => BApp(If0, Box(), Box())
      case Operator.Fold => lang.Abstract.Fold(Box(), Box(), Box())
    }
  }
  
  def getNextMinimalExpression(e: Exp, ops: List[Operator]) : Option[Exp] = e match {
    case b@Box() => if (b.isEmpty) Some(Zero()) else getNextMinimalExpression(b.e, ops)
    case Zero() => Some(One()) 
    case One() => Some(MainVar())
    case MainVar() => Some(FoldNext())
    case FoldNext() => Some(FoldAcc())
    case FoldAcc() => Some(IfZero(Box(), Box(), Box()))
    case _ => {
      val op = getOperator(e).get
      val pos = ops.indexOf(op)
      if (pos < 0)
        throw new IllegalArgumentException("Input expression uses invalid operator")
      if (pos + 1 >= ops.size)
        None
      else
        Some(getMinimalExpressionForOperator(ops(pos+1)))
    }
  }
}