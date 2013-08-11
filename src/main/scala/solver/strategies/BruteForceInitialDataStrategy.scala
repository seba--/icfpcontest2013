package solver.strategies

import solver.Strategy
import solver.Filter
import solver.Fitness
import solver.ProblemSpec
import solver.Mutator
import lang.Abstract._
import scala.collection.mutable.DoubleLinkedList
import lang.Metadata
import solver.FilterV

class BruteForceInitialDataStrategy extends Strategy {
  var isInterrupted = false
  var current: Exp = Box()

  def selfNotification(delta: Map[Long, Long]) {
    // reset: this solver requires full initial data
  }

  // Find next solution, or return None.
  def nextSolution(): Option[Exp] = {
    var next = mutator.stepInto(current)
    while (true) {
      if (isInterrupted) {
        solver.Canceled()
      } else if (!next.isDefined) {
        return None
      }

      current = next.get

      //      if (current.isInstanceOf[Fold])
      //        println(current)

      filter.filter(current) match {
        case FilterV.OK => return next
        case FilterV.STEP_INTO => next = mutator.stepInto(current)
        case FilterV.STEP_OVER => next = mutator.stepOver(current)
      }
    }
    next
  }
  def interrupt() {
    isInterrupted = true
  }
}