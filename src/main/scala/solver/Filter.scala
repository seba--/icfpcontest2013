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
  def filter(e: Exp): Boolean
}