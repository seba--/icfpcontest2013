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
import solver.ProblemSpec
import lang.Abstract.Operator._
import solver.filter.EvalNeqZeroFilter
import client.api.Problem
import lang.Semantics.Value
import solver.strategies.BruteForceStartExpStrategy
import lang.Semantics

class PartialSolver extends Solver {

  var spec: ProblemSpec = null
  var strategy: Strategy = null
  var isInterrupted = false

  def init(problem: Problem) {
    this.spec = ProblemSpec(problem)
  }

  def notifyNewData(delta: Map[Value, Value]) {
    //TODO
  }

  def nextSolution(): Option[Exp] = {
    var partitions = List[Partition]()

    for (inputOutputTuple <- spec.data) {
      if (!partitions.exists(fits(_, inputOutputTuple))) {
        if (!partitions.exists(tryFit(_, inputOutputTuple))) {
          println("Creating new partition")
          val partition = new Partition(Map(), Box())
          if (!tryFit(partition, inputOutputTuple))
            throw new IllegalStateException("Assumed to be unreachable")
          partitions = partition :: partitions
        }
      }
    }

    println("Created " + partitions.size + " partitions that are able to fit all data")
    return createExp(partitions, spec.data)
  }

  def createExp(partitions: List[Partition], unmatchedData: Map[Long, Long]): Option[Exp] = {
    if (partitions.size == 1)
      return Some(partitions.head.solution)

    for (partition <- partitions) {
      val condition = findCondition(partition, unmatchedData)
      if (condition.isDefined) {
        val subExp = createExp(partitions.filterNot(_ == partition), unmatchedData -- partition.currentData.keys)
        if (subExp.isDefined)
          return Some(IfZero(condition.get, partition.solution, subExp.get))
        else
          return None
      }
    }
    return None
  }

  def findCondition(partition: Partition, data: Map[Long, Long]): Option[Exp] = {
    val matchInput = partition.currentData.keys
    val matchNotInput = (data -- partition.currentData.keys).keys

    var matchData = Map[Long, Long]()
    matchInput.foreach(x => matchData = matchData + (x -> 0L))
    matchNotInput.foreach(x => matchData = matchData + (x -> 1L))
    createConditionStrategy(createSubProblem(matchData)).nextSolution
  }

  def fits(partition: Partition, dataPoint: (Long, Long)): Boolean = {
    if (!partition.currentData.isEmpty && Semantics.eval(partition.solution)(dataPoint._1) == dataPoint._2) {
      partition.currentData = partition.currentData + dataPoint
      println(String.format("%s fits partition: '%s'", dataPoint, partition.solution))
      return true
    }
    return false
  }

  def tryFit(partition: Partition, dataPoint: (Long, Long)): Boolean = {
    val data = partition.currentData + dataPoint
    val strategy = createStrategy(createSubProblem(data), partition.solution)
    val currentSolution = strategy.nextSolution
    if (currentSolution.isDefined) {
      println(String.format("%s fits partition: \n\tPrevious \'%s' \n\tNew: \t'%s'",
        dataPoint, partition.solution, currentSolution.get))
      partition.currentData = data
      partition.solution = currentSolution.get
      return true
    } else {
      println(String.format("Cannot fit %s in solution %s", dataPoint, partition.solution))
      return false
    }
  }

  def createSubProblem(data: Map[Long, Long]): ProblemSpec = {
    new ProblemSpec(spec.id, spec.size, spec.operators.filterNot(_ == If0), data)
  }

  def createStrategy(spec: ProblemSpec, initial: Exp): Strategy = {
    val strategy = new BruteForceStartExpStrategy(initial)
    val filter = new CompositeFilter(List(new SizeFilter, new EvalFilter))
    strategy.init(spec, LinearMutator, filter, ConstantFitness(1.0))
    return strategy
  }

  def createConditionStrategy(spec: ProblemSpec): Strategy = {
    val strategy = new BruteForceInitialDataStrategy
    val filter = new CompositeFilter(List(new SizeFilter, new EvalNeqZeroFilter))
    strategy.init(spec, LinearMutator, filter, ConstantFitness(1.0))
    return strategy
  }

  def interrupt() {
    isInterrupted = true
  }

  class Partition(
    var currentData: Map[Long, Long] = Map(),
    var solution: Exp = null) {
  }

}