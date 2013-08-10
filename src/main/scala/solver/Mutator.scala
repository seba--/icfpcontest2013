package solver

import lang.Abstract._

/**
 * Mutates one (partial) program to the next
 */
trait Mutator {
  // initialize
  def init(spec: ProblemSpec): Unit
  // notify about new evaluation data, will be already registered in problem spec
  def notifyNewData(data: Map[Long, Long]): Unit
  
  // continue mutating this version of the program; return None if no valid mutation is possible
  def stepInto(e: Exp): Option[Exp]

  // the argument program is invalid: skip it and produce another mutation; return None if no valid mutation is possible
  def stepOver(e: Exp): Option[Exp]
}