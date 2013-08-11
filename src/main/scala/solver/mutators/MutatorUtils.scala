package solver.mutators

import lang.Abstract._
import lang.Abstract.Operator._

object MutatorUtils {
  def getMinimalExpressionForOperator(op:Operator) : Exp = {
    op match {
      case Not => UApp(Not, MainVar)
      case Shl1 => UApp(Shl1, One)
      case Shr1 => UApp(Shr1, MainVar)
      case Shr4 => UApp(Shr4, MainVar)
      case Shr16 => UApp(Shr16, MainVar)
      case And => BApp(And, Zero, One)
      case Or => BApp(Or, Zero, One)
      case Plus => BApp(Plus, Zero, One)
      case Xor => BApp(Xor, Zero, One)
      case If0 => IfZero(MainVar, One, Zero)
      case Operator.Fold => lang.Abstract.Fold(Zero, Zero, FoldAcc)
      case Operator.TFold => lang.Abstract.Fold(Zero, Zero, Zero)
    }
  }
  
  def getNextMinimalExpression(e: Exp, ops: List[Operator]) : Option[Exp] = e match {
    case b@Box() => if (b.isEmpty) Some(Zero) else getNextMinimalExpression(b.e, ops)
    case Zero => Some(One) 
    case One => Some(MainVar)
    case MainVar => Some(FoldAcc)
    case FoldAcc => Some(FoldNext)
    case FoldNext => Some(getMinimalExpressionForOperator(ops.head))
    case _ => {
      val op = getOperator(e).get
      var pos = ops.indexOf(op)
      if (pos < 0 && op == Operator.Fold)
        pos = ops.indexOf(Operator.TFold)
      if (pos < 0)
        throw new IllegalArgumentException("Input expression uses invalid operator")
      if (pos + 1 >= ops.size)
        None
      else
        Some(getMinimalExpressionForOperator(ops(pos+1)))
    }
  }
}