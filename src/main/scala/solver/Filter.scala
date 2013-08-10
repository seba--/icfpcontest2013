package solver

import lang.Abstract.Exp

/**
 * Filter intermediate programs.
 */
trait Filter {
  // initialize
  def init(spec: ProblemSpec): Unit
  // notify about new evaluation data, will be already registered in problem spec
  def notifyNewData(data: Map[Long, Long]): Unit

  // keep all expressions that satisfy the predicate
  def filter(e: Exp): Int
}

object FilterV {
  val OK = 0
  val STEP_INTO = 1
  val STEP_OVER = 2
  def max(v: Int, f:(Exp) => Int, e: Exp): Int = {
    if (v == STEP_OVER)
      STEP_OVER
    else
      Math.max(v, f(e))
  }
  def max_(f1:(Exp) => Int, f2:(Exp) => Int, e:Exp): Int = {
    max(f1(e), f2, e)
  }
}
