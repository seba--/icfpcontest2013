package solver

import lang.FlatAbstract._

/**
 * Search solutions using a mutator, filter, and fitness function.
 */
trait Solver {
  // Initialize.
  def init(spec: ProblemSpec, mutator: Mutator, filter: Filter, fitness: Fitness)
  // Find next solution, or return None.
  def nextSolution(): Option[Exp]
}