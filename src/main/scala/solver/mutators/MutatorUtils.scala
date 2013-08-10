package solver.mutators

import lang.Abstract._
import lang.Abstract.Operator._

object MutatorUtils {
  def getMinimalExpressionForOperator(op:Operator) : Exp = {
    op match {
      case Not => UApp(Not, Zero())
      case Shl1 => UApp(Shl1, Zero())
      case Shr1 => UApp(Shr1, Zero())
      case Shr4 => UApp(Shr4, Zero())
      case Shr16 => UApp(Shr16, Zero())
      case And => BApp(And, Zero(), Zero())
      case Or => BApp(Or, Zero(), Zero())
      case Plus => BApp(Plus, Zero(), Zero())
      case Xor => BApp(Xor, Zero(), Zero())
      case If0 => IfZero(Zero(), Zero(), Zero())
      case Operator.Fold => lang.Abstract.Fold(Zero(), Zero(), Zero())
    }
  }
  
  def getNextMinimalExpression(e: Exp, ops: List[Operator]) : Option[Exp] = e match {
    case b@Box() => if (b.isEmpty) Some(Zero()) else getNextMinimalExpression(b.e, ops)
    case Zero() => Some(One()) 
    case One() => Some(MainVar())
    case MainVar() => Some(FoldAcc())
    case FoldAcc() => Some(FoldNext())
    case FoldNext() => Some(getMinimalExpressionForOperator(ops.head))
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