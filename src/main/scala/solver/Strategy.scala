package solver

import lang.Abstract._

/**
 * Search solutions using a mutator, filter, and fitness function.
 */
trait Strategy {
  // Initialize.
  def init(spec: ProblemSpec, mutator: Mutator, filter: Filter, fitness: Fitness) 
  // notify about new evaluation data, will be already registered in problem spec
  def notifyNewData(delta: Map[Long, Long]): Unit
  // Find next solution, or return None.
  def nextSolution(): Option[Exp]
  def interrupt()
}