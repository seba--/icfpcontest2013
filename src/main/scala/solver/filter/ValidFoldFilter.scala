package solver.filter

import scala.collection.mutable.DoubleLinkedList
import solver.Filter
import scala.collection.immutable.Map
import solver.ProblemSpec
import lang.Abstract._
import solver.FilterV

/**
 * Ensures that the expression contains at most 1 fold operator.
 */
class ValidFoldFilter extends Filter {

  def init(spec: ProblemSpec) {
    // ignore
  }

  def notifyNewData(delta: Map[Long, Long]) {
    // ignore
  }

  def filter(e: Exp): Int = {
    filter(e, false)
  }

  def filter(e: Exp, inFold: Boolean): Int = {
    e match {
      case IfZero(cond, e1, e2) => Math.max(filter(cond, inFold), Math.max(filter(e1, inFold), filter(e2, inFold)))
      case Fold(over, init, body) => {
        if (inFold)
          FilterV.STEP_OVER
        else {
          Math.max(filter(over, true), Math.max(filter(init, true), filter(body, true))) 
        }
      }
      case FoldNext() => if (inFold) FilterV.OK else FilterV.STEP_INTO
      case FoldAcc() => if (inFold) FilterV.OK else FilterV.STEP_INTO
      case UApp(op, e1) => filter(e1, inFold)
      case BApp(op, e1, e2) => Math.max(filter(e1, inFold), filter(e2, inFold))
      case b @ Box() => if (b.isEmpty) FilterV.OK else filter(b.e, inFold)
      case _ => FilterV.OK
    }
  }
}