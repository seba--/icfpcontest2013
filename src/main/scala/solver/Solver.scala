package solver

import lang.FlatAbstract._

/**
 * An independent solver for a problem specification.  
 */
trait Solver {
  def init(spec: ProblemSpec)
  def nextSolution(): Option[Exp]
}