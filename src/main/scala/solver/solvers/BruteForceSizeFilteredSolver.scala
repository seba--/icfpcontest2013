package solver.solvers

import solver.Solver
import solver.ProblemSpec
import solver.Strategy
import solver.strategies.BruteForceInitialDataStrategy
import solver.mutators.LinearMutator
import solver.filter.SizeFilter
import solver.fitness.ConstantFitness
import lang.FlatAbstract._

class BruteForceSizeFilteredSolver extends Solver {
  var strategy: Strategy = null
  
  def init(spec: ProblemSpec) {
    strategy = new BruteForceInitialDataStrategy
    strategy.init(spec, LinearMutator, new SizeFilter, ConstantFitness(1.0))
  }
  
  def nextSolution(): Option[Exp] =
    strategy.nextSolution
}