package solver

import lang.Abstract._
import lang.Semantics.Value
import client.api.Problem

object Canceled extends Exception {
  def apply() = throw this
}

/**
 * An independent solver for a problem specification.
 */
trait Solver {
  def init(problem: Problem)
  // notify about new evaluation data, will be already registered in problem spec
  def notifyNewData(delta: Map[Value, Value]): Unit
  def nextSolution(): Option[Exp]
  def interrupt()
}