package solver.solvers

import solver.Solver
import solver.ProblemSpec
import solver.Strategy
import solver.strategies.BruteForceInitialDataStrategy
import solver.mutators.LinearMutator
import solver.filter._
import solver.fitness.ConstantFitness
import lang.Abstract._
import solver.fitness.ConstantFitness

class BruteForceSizeFilteredSolver extends Solver {
  var strategy: Strategy = null
  
  def init(spec: ProblemSpec) {
    strategy = new BruteForceInitialDataStrategy
    val filters = List(
      new SizeFilter,
      new ValidFoldFilter,
      new ConstantFoldingFilter,
      new ShortcutShiftFilter,
      new IdentityOpFilter,
      new TFoldConditionFilter,
      new EvalFilter)
    strategy.init(spec, LinearMutator, new CompositeFilter(filters), ConstantFitness(1.0))
  }
  
  def notifyNewData(delta: Map[Long, Long]) = strategy.notifyNewData(delta)

  def nextSolution(): Option[Exp] =
    strategy.nextSolution
}
