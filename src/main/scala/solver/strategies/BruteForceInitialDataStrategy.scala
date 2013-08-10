package solver.strategies

import solver.Strategy
import lang.Abstract._
import lang.Metadata
import solver.FilterV

class BruteForceInitialDataStrategy extends Strategy {
  var current: Exp = Box()

  def notifyNewData(delta: Map[Long, Long]) {
    // reset: this solver requires full initial data
    current = Box()
  }

  // Find next solution, or return None.
  def nextSolution(): Option[Exp] = {
    var next = mutator.stepInto(current)
    var count = 0
    while (true) {
      if (!next.isDefined)
        return None


      if (Metadata.ops(current).contains(Operator.If0) && Metadata.size(current) == 25 && (count % 1000) == 0)
        println(current)

      count +=1

      current = next.get
      filter.filter(current) match {
        case FilterV.OK => return next
        case FilterV.STEP_INTO => next = mutator.stepInto(current)
        case FilterV.STEP_OVER => next = mutator.stepOver(current)
      }
    }
    next
  }
}