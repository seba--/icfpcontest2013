package solver.solvers

import solver.Solver
import solver.ProblemSpec
import solver.Strategy
import solver.strategies.BruteForceInitialDataStrategy
import solver.mutators.LinearMutator
import solver.filter._
import solver.fitness.ConstantFitness
import lang.Abstract._
<<<<<<< HEAD
import solver.fitness.ConstantFitness
=======
import solver.filter.CompositeFilter
import solver.filter.EvalFilter
import solver.filter.ValidFoldFilter
import client.api.Problem
>>>>>>> 491c401ac4cea547577d740ebff09614e577975f

class BruteForceSizeFilteredSolver extends Solver {
  var strategy: Strategy = null
  // TODO get rid of ProblemSpec class??
  var problemSpec : ProblemSpec = _
  
  def init(problem: Problem) {
    problemSpec = ProblemSpec(problem)
    strategy = new BruteForceInitialDataStrategy
    val filters = List(
      new SizeFilter,
      new ValidFoldFilter,
      new ConstantFoldingFilter,
      new ShortcutShiftFilter,
      new IdentityOpFilter,
      new TFoldConditionFilter,
      new EvalFilter)
    strategy.init(problemSpec, LinearMutator, new CompositeFilter(filters), ConstantFitness(1.0))
  }
  
  def notifyNewData(delta: Map[Long, Long]) {
    problemSpec.data ++= delta
    strategy.notifyNewData(delta)
  }

  def nextSolution(): Option[Exp] =
    strategy.nextSolution
}
