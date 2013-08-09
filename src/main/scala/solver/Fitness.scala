package solver

import lang.FlatAbstract.Exp

/**
 * Compute fitness of intermediate solution
 */
trait Fitness {
  // initialize
  def init(spec: ProblemSpec): Unit
  // notify about new evaluation data, will be already registered in problem spec
  def notifyNewData(data: Map[Long, Long]): Unit
  // returns fitness *normalized* to values in range [0,1]
  def fitness(e: Exp): Double
}