package solver.fitness

import lang.FlatAbstract._
import solver.Fitness
import solver.ProblemSpec

case class ConstantFitness(fitness: Double) extends Fitness {
  def init(spec: ProblemSpec) {}

  def notifyNewData(data: Map[Long, Long]) {}

  def fitness(e: Exp) = fitness
}