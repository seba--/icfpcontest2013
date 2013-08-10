package solver

import lang.Abstract._
import lang.Semantics.Value
import client.api.Problem

/**
 * An independent solver for a problem specification.
 */
trait Solver {
  def init(problem: Problem)
  // notify about new evaluation data, will be already registered in problem spec
  def notifyNewData(delta: Map[Value, Value]): Unit
  def nextSolution(): Option[Exp]
}