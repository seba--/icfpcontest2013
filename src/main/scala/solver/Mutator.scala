package solver

import lang.FlatAbstract.Node
import lang.FlatAbstract.Exp

/**
 * Mutates one (partial) program to the next
 */
trait Mutator {
  // initialize
  def init(spec: ProblemSpec): Unit
  // notify about new evaluation data, will be already registered in problem spec
  def notifyNewData(data: Map[Long, Long]): Unit
  // get mutated version of program, or None if no valid mutation is possible
  def mutate(e: Exp): Option[Exp]
}