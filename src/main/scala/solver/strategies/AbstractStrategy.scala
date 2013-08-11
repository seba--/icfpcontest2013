package solver.strategies

import solver.Fitness
import solver.Filter
import solver.Mutator
import solver.ProblemSpec
import solver.Strategy

abstract class AbstractStrategy extends Strategy {
  var spec: ProblemSpec = null
  var mutator: Mutator = null
  var filter: Filter = null
  var fitness: Fitness = null

  // Initialize.
  def init(spec: ProblemSpec, mutator: Mutator, filter: Filter, fitness: Fitness) {
    this.spec = spec
    this.mutator = mutator
    this.filter = filter
    this.fitness = fitness

    mutator.init(spec)
    filter.init(spec)
    fitness.init(spec)
  }
  // notify about new evaluation data, will be already registered in problem spec
  def notifyNewData(delta: Map[Long, Long]): Unit = {
    mutator.notifyNewData(delta)
    filter.notifyNewData(delta)
    fitness.notifyNewData(delta)
  }
}