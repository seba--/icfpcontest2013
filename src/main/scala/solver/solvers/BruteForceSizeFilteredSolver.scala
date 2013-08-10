package solver.solvers

import solver.Solver
import solver.ProblemSpec
import solver.Strategy
import solver.strategies.BruteForceInitialDataStrategy
import solver.mutators.LinearMutator
import solver.filter.SizeFilter
import solver.fitness.ConstantFitness
import lang.Abstract._
import solver.filter.CompositeFilter
import solver.filter.EvalFilter
import solver.filter.ValidFoldFilter

class BruteForceSizeFilteredSolver extends Solver {
  var strategy: Strategy = null
  
  def init(spec: ProblemSpec) {
    strategy = new BruteForceInitialDataStrategy
    val filters = List(new SizeFilter, new ValidFoldFilter, new EvalFilter)
    strategy.init(spec, LinearMutator, new CompositeFilter(filters), ConstantFitness(1.0))
  }
  
  def nextSolution(): Option[Exp] =
    strategy.nextSolution
}