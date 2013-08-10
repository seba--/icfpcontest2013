package solver.filter

import solver.Filter
import solver.ProblemSpec
import lang.Abstract._
import lang.Semantics._

/**
 * Ensures that the expression is a valid language expression.
 * Ensures that eval(expression)(input) == output for all known input-output pairs.
 */
class EvalFilter extends Filter {
  var spec: ProblemSpec = null

  def init(spec: ProblemSpec) {
    this.spec = spec
  }

  def notifyNewData(delta: Map[Long, Long]) {
    // ignore
  }

  def filter(e: Exp): Boolean = {
    try {
      return evaluate(e)
    } catch {
      case _: Exception => return false
    }
  }

  private def evaluate(e: Exp): Boolean = {
    val p = eval(e)(_)
    spec.data foreach {
      t => if (p(t._1) != t._2) return false;
    }
    return true;
  }

}