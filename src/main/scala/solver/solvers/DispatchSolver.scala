package solver
package solvers

import client.api.Problem
import lang.Abstract.Operator._
import lang.Abstract.Exp
import datacollection.BotApp

class DispatchSolver extends Solver {
  val bruteForce = new BruteForceSizeFilteredSolver
  val partial = new PartialSolver

  var currentSolver: Solver = _
  def init(problem: Problem) = {
    currentSolver = if (problem.operators.contains(TFold) || !problem.operators.contains(If0)) {
      BotApp.log("[Dispatch] delegating to BruteForce")
      bruteForce
    } else {
      BotApp.log("[Dispatch] delegating to Partial")
      partial
    }
    currentSolver.init(problem)
  }
  def notifyNewData(delta: Map[Long, Long]) {
    currentSolver.notifyNewData(delta)
  }
  def nextSolution(): Option[Exp] = {
    currentSolver.nextSolution
  }
  def interrupt() = {
    currentSolver.interrupt
  }
}