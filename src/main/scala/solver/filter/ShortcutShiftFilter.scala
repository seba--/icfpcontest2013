package solver.filter

import solver.Filter
import solver.FilterV
import solver.ProblemSpec
import lang.Abstract._
import lang.Abstract.Operator._

class ShortcutShiftFilter extends Filter {

  var specs: ProblemSpec = null

  // initialize
  def init(spec: ProblemSpec): Unit = { specs = spec }

  // notify about new evaluation data, will be already registered in problem spec
  def notifyNewData(data: Map[Long, Long]): Unit = {}

  // keep all expressions that satisfy the predicate
  def filter(e: Exp): Int = {
    e match {
      case UApp(Shr1, UApp(Shr1, UApp(Shr1, UApp(Shr1, _)))) if (specs.operators.contains(Shr4)) => FilterV.STEP_OVER
      case UApp(Shr4, UApp(Shr4, UApp(Shr4, UApp(Shr4, _)))) if (specs.operators.contains(Shr16)) => FilterV.STEP_OVER
      case UApp(Shr16, UApp(Shr16, UApp(Shr16, UApp(Shr16, _)))) => FilterV.STEP_OVER
      case _ => FilterV.OK
    }
  }
}