package solver.solvers

import solver.Solver
import solver.ProblemSpec
import solver.Strategy
import solver.strategies.BruteForceInitialDataStrategy
import solver.mutators._
import solver.filter._
import lang.Abstract._
import solver.fitness.ConstantFitness
import solver.filter.CompositeFilter
import solver.filter.EvalFilter
import solver.filter.ValidFoldFilter
import client.api.Problem

class BruteForceSizeFilteredSolver extends Solver {
  var strategy: Strategy = null
  // TODO get rid of ProblemSpec class??
  var problemSpec: ProblemSpec = _

  def init(problem: Problem) {
    problemSpec = ProblemSpec(problem)
    strategy = new BruteForceInitialDataStrategy
    val filters = List(
      // STEP_OVER-Filter
      new SizeFilter, // OVER
      new ShortcutShiftFilter, //OVER
      // both-STEP-Filter (at most one)
      new ValidFoldFilter, //both
      // STEP_INTO-Filter
      new TFoldConditionFilter, //both
      new ConstantFoldingFilter, //INTO
      new IdentityOpFilter, //INTO
      //new BinaryComparisonFilter, //INTO
      new EvalFilter //INTO
      )
    strategy.init(problemSpec, new TFoldMutatorDecorator(LinearMutator), new CompositeFilter(filters), ConstantFitness(1.0))
  }

  def notifyNewData(delta: Map[Long, Long]) {
    problemSpec.data ++= delta
    strategy.notifyNewData(delta)
  }

  def nextSolution(): Option[Exp] =
    strategy.nextSolution

  def interrupt() = strategy.interrupt()
}
