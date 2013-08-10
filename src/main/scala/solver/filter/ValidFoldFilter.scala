package solver.filter

import scala.collection.mutable.DoubleLinkedList
import solver.Filter
import scala.collection.immutable.Map
import solver.ProblemSpec
import lang.Abstract._

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

  def filter(e: Exp): Boolean = {
    filter(e, false)
  }

  def filter(e: Exp, inFold: Boolean): Boolean = {
    e match {
      case IfZero(cond, e1, e2) => filter(cond, inFold) && filter(e1, inFold) && filter(e2, inFold)
      case Fold(over, init, body) => {
        if (inFold)
          false
        else {
          filter(over, true) && filter(init, true) && filter(body, true) 
        }
      }
      case FoldNext() => true
      case FoldAcc() => true
      case UApp(op, e1) => filter(e1, inFold)
      case BApp(op, e1, e2) => filter(e2, filter(e1, inFold))
      case b @ Box() => if (b.isEmpty) true else filter(b.e, inFold)
      case _ => true
    }
  }
}