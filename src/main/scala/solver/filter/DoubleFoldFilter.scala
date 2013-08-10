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

  def init(spec: ProblemSpec) {
    this.spec = spec
  }

  def notifyNewData(delta: Map[Long, Long]) {
    // ignore
  }

  def filter(e: Exp): Boolean = {
    filter(e, false)
  }
  
  def filter(e: Exp, hasFold: Boolean): Boolean = {
    e match {
      case IfZero(cond, e1, e2) => filter(cond, false) && filter(e1, false) && filter(e2, false) 
      case Fold(over, init, body) => !hasFold && filter(over, true) && filter(init, true) && filter(body, true)
      case UApp(op, e1) => filter(e1, false)
      case BApp(op, e1, e2) => filter(e1, false) && filter(e2, false)
      case b@Box() => if (b.isEmpty) true else filter(b.e, false)
      case _ => true
    }
  }
}