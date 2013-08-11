package solver.filter

import scala.collection.immutable.Map
import lang.Abstract.BApp
import lang.Abstract.Box
import lang.Abstract.Exp
import lang.Abstract.Fold
import lang.Abstract.IfZero
import lang.Abstract.MainVar
import lang.Abstract.Operator
import lang.Abstract.UApp
import lang.Abstract.Zero
import solver.Filter
import solver.ProblemSpec
import solver.FilterV

/**
 * Ensures that, if the problem specification names tfold as one of the operators,
 * the expression has a fold on the outermost-operation and the main variable
 * appears exactly once, as the argument of the fold).
 */
class TFoldConditionFilter extends Filter {
  
  var isTFold : Boolean = false

  def init(spec: ProblemSpec) {
    isTFold = spec.operators.contains(Operator.TFold)
  }

  def notifyNewData(delta: Map[Long, Long]) {
    // ignore
  }

  def filter(e: Exp): Int = e match {
    case Fold(over, init, body) if isTFold =>
      if (over == MainVar() && init == Zero() && !containsMainVar(body))
        FilterV.OK
      else
        FilterV.STEP_INTO
    case _ if isTFold => FilterV.STEP_OVER

    case Fold(over, init, body) if !isTFold =>
      if (over == MainVar() && init == Zero() && !containsMainVar(body))
        FilterV.STEP_INTO
      else
        FilterV.OK
    case _ => FilterV.OK
  }

  def filterFirstFold(e: Exp): Boolean = e match {
    case Fold(MainVar(), Zero(), _) => false
    case _ => true
  }

  private def containsMainVar(e: Exp): Boolean = e match {
    case IfZero(cond, e1, e2) =>
      containsMainVar(cond) && containsMainVar(e1) && containsMainVar(e2)
    case Fold(over, init, body) =>
      containsMainVar(over) && containsMainVar(init) && containsMainVar(body)
    case UApp(op, e1) =>
      containsMainVar(e1)
    case BApp(op, e1, e2) =>
      containsMainVar(e1) && containsMainVar(e2)
    case b @ Box() =>
      if (b.isEmpty) false else containsMainVar(b.e)
    case MainVar() =>
      true
    case _ =>
      false
  }
}