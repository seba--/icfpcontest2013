package solver

import lang.Abstract._

/**
 * An independent solver for a problem specification.
 */
trait Solver {
  def init(spec: ProblemSpec)
  // notify about new evaluation data, will be already registered in problem spec
  def notifyNewData(delta: Map[Long, Long]): Unit
  def nextSolution(): Option[Exp]
}