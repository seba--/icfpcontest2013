package solver.strategies

import solver.Strategy
import solver.Filter
import solver.Fitness
import solver.ProblemSpec
import solver.Mutator
import lang.Abstract._
import scala.collection.mutable.DoubleLinkedList

class BruteForceInitialDataStrategy extends Strategy {
  var current: Exp = Box()
  
  def notifyNewData(delta: Map[Long, Long]) {
    // reset: this solver requires full initial data
    current = Box()
  }

  // Find next solution, or return None.
  def nextSolution(): Option[Exp] = {
    val next = mutator.mutate(current)
    if (!next.isDefined)
      return None
    current = next.get
    if (!filter.filter(current))
      return next
    return nextSolution()
  }
}