package solver

import lang.FlatAbstract.Exp

/**
 * Filter intermediate programs.
 */
trait Filter {
  // initialize
  def init(spec: ProblemSpec): Unit
  // notify about new evaluation data, will be already registered in problem spec
  def notifyNewData(data: Map[Long, Long]): Unit
  // reject expression if return true
  def filter(e: Exp): Boolean
}