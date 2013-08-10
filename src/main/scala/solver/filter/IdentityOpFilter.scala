package solver.filter

import solver.Filter
import solver.ProblemSpec
import lang.Abstract._
import lang.Abstract.Operator._

class IdentityOpFilter extends Filter {

  // initialize
  def init(spec: ProblemSpec): Unit = {}

  // notify about new evaluation data, will be already registered in problem spec
  def notifyNewData(data: Map[Long, Long]): Unit = {}

  // keep all expressions that satisfy the predicate
  def filter(e: Exp): Boolean = {
    e match {
      case BApp(Plus, Zero(), _) => false
      case BApp(Plus, _, Zero()) => false
      case BApp(And, Zero(), _) => false
      case BApp(And, _, Zero()) => false
      case BApp(Or, Zero(), _) => false
      case BApp(Or, _, Zero()) => false
      case UApp(Not, UApp(Not, _)) => false
      case _ => true
    }
  }
}