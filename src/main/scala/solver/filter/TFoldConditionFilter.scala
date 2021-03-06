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
abstract class TFoldFilter extends Filter {
  
  var is_TFold : Boolean = false
  def isTFold : Boolean = is_TFold

  def init(spec: ProblemSpec) {
    is_TFold = spec.operators.contains(Operator.TFold)
  }

  def notifyNewData(delta: Map[Long, Long]) {
    // ignore
  }

  def filter(e: Exp): Int

  def filterFirstFold(e: Exp): Boolean = e match {
    case Fold(MainVar, Zero, _) => false
    case _ => true
  }

  def containsMainVar(e: Exp): Boolean = e match {
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
    case MainVar =>
      true
    case _ =>
      false
  }
}

class TFoldConditionFilter extends TFoldFilter {
  def filter(e: Exp): Int = e match {
    case Fold(over, init, body) if isTFold =>
      if (over == MainVar && init == Zero && !containsMainVar(body))
        FilterV.OK
      else
        FilterV.STEP_INTO
    case _ if isTFold => FilterV.STEP_INTO // should be OVER but the ExistenceFilter does this and this change was necessary to gain speedup

    case Fold(over, init, body) if !isTFold =>
      if (over == MainVar && init == Zero && !containsMainVar(body))
        FilterV.STEP_INTO
      else
        FilterV.OK
    case _ => FilterV.OK
  }
}

class TFoldExistenceFilter extends TFoldFilter {
  def filter(e: Exp): Int = e match {
    case Fold(over, init, body) if isTFold => FilterV.OK
    case _ if isTFold => FilterV.STEP_OVER
    case _ => FilterV.OK
  }
}