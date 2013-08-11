package solver.filter

import solver.Filter
import solver.FilterV
import solver.ProblemSpec
import lang.Abstract._
import lang.Abstract.Operator._

class IdentityOpFilter extends Filter {

  // initialize
  def init(spec: ProblemSpec): Unit = {}

  // notify about new evaluation data, will be already registered in problem spec
  def notifyNewData(data: Map[Long, Long]): Unit = {}

  // keep all expressions that satisfy the predicate
  def filter(e: Exp): Int = {
    e match {
      case BApp(Plus, Zero, _) => FilterV.STEP_INTO
      case BApp(Plus, _, Zero) => FilterV.STEP_INTO
      case BApp(And, Zero, _) => FilterV.STEP_INTO
      case BApp(And, _, Zero) => FilterV.STEP_INTO
      case BApp(Or, Zero, _) => FilterV.STEP_INTO
      case BApp(Or, _, Zero) => FilterV.STEP_INTO
      case UApp(Not, UApp(Not, _)) => FilterV.STEP_INTO
      case b @ Box() => if (b.isEmpty) FilterV.OK else filter(b.e)
      case UApp(_, e) => filter(e)
      case BApp(_, e1, e2) => FilterV.or(filter(e1), filter, e2)
      case IfZero(cond, yes, no) => FilterV.or(FilterV.or(filter(cond), filter, yes), filter, no)
      case lang.Abstract.Fold(over, init, body) => FilterV.or(FilterV.or(filter(over), filter, init), filter, body)
      case _ => FilterV.OK
    }
  }
}