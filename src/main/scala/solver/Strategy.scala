package solver

import lang.Abstract._

/**
 * Search solutions using a mutator, filter, and fitness function.
 */
abstract class Strategy {
  var spec: ProblemSpec = null
  var mutator: Mutator = null
  var filter: Filter = null
  var fitness: Fitness = null

  // Initialize.
  def init(spec: ProblemSpec, mutator: Mutator, filter: Filter, fitness: Fitness) {
	Strategy.this.spec = spec
    Strategy.this.mutator = mutator
	Strategy.this.filter = filter
	Strategy.this.fitness = fitness
	
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
  def selfNotification(delta: Map[Long, Long]): Unit
  // Find next solution, or return None.
  def nextSolution(): Option[Exp]
  def interrupt()
}