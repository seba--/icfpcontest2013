package solver.filter

import solver.Filter
import solver.FilterV
import solver.ProblemSpec
import lang.Abstract._
import lang.Semantics._

/**
 * - Ensures that the expression is a valid (partial) language expression.
 * - Ensures that eval(expression)(input) == 0 for all
 *   input-output pairs of the form (x,0) and eval(expression)(input) != 0 for all other pairs
 */
class EvalNeqZeroFilter extends Filter {
  var spec: ProblemSpec = null

  def init(spec: ProblemSpec) {
    this.spec = spec
  }

  def notifyNewData(delta: Map[Long, Long]) {
    // ignore
  }

  def filter(e: Exp): Int = {
    try {
      if (evaluate(e))
        FilterV.OK
      else
        FilterV.STEP_INTO
    } catch {
      case EmptyBox() => FilterV.OK
      case ex: Exception => {
        FilterV.STEP_INTO
      }
    }
  }

  private def evaluate(e: Exp): Boolean = {
    val p = eval(e)(_)
    spec.data foreach {
      _ match {
        case (x, 0L) => if (p(x) != 0) return false;
        case (y, _) => if (p(y) == 0) return false;
      }
    }
    return true;
  }

}
