package solver

import lang.Abstract._

/**
 * An independent solver for a problem specification.  
 */
trait Solver {
  def init(spec: ProblemSpec)
  def nextSolution(): Option[Exp]
}