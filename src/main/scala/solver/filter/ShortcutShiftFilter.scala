package solver.filter

import solver.Filter
import solver.ProblemSpec
import lang.Abstract._
import lang.Abstract.Operator._

class ShortcutShiftFilter extends Filter {

  // initialize
  def init(spec: ProblemSpec): Unit = {}

  // notify about new evaluation data, will be already registered in problem spec
  def notifyNewData(data: Map[Long, Long]): Unit = {}

  // keep all expressions that satisfy the predicate
  def filter(e: Exp): Boolean = {
    e match {
      case UApp(Shr1, UApp(Shr1, UApp(Shr1, UApp(Shr1, _)))) => false
      case UApp(Shr4, UApp(Shr4, UApp(Shr4, UApp(Shr4, _)))) => false
      case UApp(Shr16, UApp(Shr16, UApp(Shr16, UApp(Shr16, _)))) => false
    }
  }
}