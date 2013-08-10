package solver.filter

import scala.collection.mutable.DoubleLinkedList
import solver.Filter
import scala.collection.immutable.Map
import solver.ProblemSpec
import lang.Abstract._

/**
 * Ensures that the expression contains at most 1 fold operator.
 */
class DoubleFoldFilter extends Filter {
  var spec: ProblemSpec = null
  var hasFold: Boolean = false

  def init(spec: ProblemSpec) {
    this.spec = spec
  }

  def notifyNewData(delta: Map[Long, Long]) {
    // ignore
  }

  def filter(e: Exp): Boolean = {
    hasFold = false
    filter(e, false)
  }

  def filter(e: Exp, inFold: Boolean): Boolean = {
    e match {
      case IfZero(cond, e1, e2) => filter(cond, inFold) && filter(e1, inFold) && filter(e2, inFold)
      case Fold(over, init, body) => {
        if (hasFold)
          false
        else {
          hasFold = true
          filter(over, inFold) && filter(init, inFold) && filter(body, true)
        }
      }
      case FoldNext() => inFold
      case FoldAcc() => inFold
      case UApp(op, e1) => filter(e1, inFold)
      case BApp(op, e1, e2) => filter(e1, inFold) && filter(e2, inFold)
      case b @ Box() => if (b.isEmpty) true else filter(b.e, inFold)
      case _ => true
    }
  }
}