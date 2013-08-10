package solver.filter

import scala.collection.mutable.DoubleLinkedList
import solver.Filter
import scala.collection.immutable.Map
import solver.ProblemSpec
import lang.Abstract._

/**
 * Ensures that, if the problem specification names tfold as one of the operators,
 * the expression has a fold on the outermost-operation and the main variable
 * appears exactly once, as the argument of the fold).
 */
class TFoldConditionFilter extends Filter {
  var spec: ProblemSpec = null

  def init(spec: ProblemSpec) {
    this.spec = spec
  }

  def notifyNewData(delta: Map[Long, Long]) {
    // ignore
  }

  def filter(e: Exp): Boolean = {
    if (spec.operators.contains(Operator.TFold)) {
      e match {
        case Fold(over, init, body) =>
          over == MainVar() && init == Zero() && !containsMainVar(body)
        case _ => false
      }
    } else {
      true
    }
  }
  
  private def containsMainVar(e : Exp) : Boolean = e match {
      case IfZero(cond, e1, e2) =>
        containsMainVar(cond) && containsMainVar(e1) && containsMainVar(e2) 
      case Fold(over, init, body) =>
        containsMainVar(over) && containsMainVar(init) && containsMainVar(body)
      case UApp(op, e1) =>
        containsMainVar(e1)
      case BApp(op, e1, e2) =>
        containsMainVar(e1) && containsMainVar(e2)
      case b@Box() =>
        if (b.isEmpty) false else containsMainVar(b.e)
      case MainVar() =>
        true
      case _ =>
        false
    }
}