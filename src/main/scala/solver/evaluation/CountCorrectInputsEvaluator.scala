package solver
package evaluation

import client.api.Problem
import lang.Semantics
import datacollection.BotApp._
import lang.Abstract.Exp

class EvaluatedSolution(nextSolutionBenchmark: Benchmarked[Exp], problem: Problem) {
  val Benchmarked(program, duration) = nextSolutionBenchmark
  val counts = EvaluationResultCounter(program, problem)
  val EvaluationResultCounter(correct, wrong, crashed) = counts
}

case class EvaluationResultCounter(correct: Int, wrong: Int, crashed: Int) {
  def countCorrect() = copy(correct = correct + 1)
  def countWrong() = copy(wrong = wrong + 1)
  def countCrashed() = copy(crashed = crashed + 1)
}

object EvaluationResultCounter {
  def apply(program: Exp, problem: Problem): EvaluationResultCounter = {
    problem.evaluationResults.get.foldLeft(EvaluationResultCounter(0, 0, 0)) {
      case (counter, (in, out)) =>
        try {
          if (Semantics.eval(program)(in) == out) {
            counter.countCorrect
          } else {
            counter.countWrong
          }
        } catch {
          case e: Exception =>
            counter.countCrashed
        }
    }
  }
}

class CountCorrectInputsEvaluator(problems: Iterable[Problem]) extends SolverEvaluator {
  override def evaluate(solver: Solver) {
    var i = 0
    var correct = 0
    problems.map(problem => problem -> {
      i += 1
      log("Test " + i + " of " + problems.size)
      val solCount = evaluate(problem, solver)
      if (solCount > 0)
        correct += 1
    })
    log("Solved " + correct + " of " + problems.size)
  }

  def evaluate(problem: Problem, solver: Solver) = {
    log("starting init for problem: %s, program size: %d, operators: %s".format(problem.id, problem.size, problem.operators))
    log("init took %dms".format(Benchmarked(solver.init(problem)).duration))
    var continue = true;
    var count = 0
    do {
      continue = try {
        val benchmarked = Benchmarked(solver.nextSolution())
        if (benchmarked.value.isDefined) {
          val evaled = new EvaluatedSolution(benchmarked.map { _.get }, problem)
          val EvaluationResultCounter(correct, wrong, crashed) = evaled.counts
          log("next solution took %dms, results: %d correct, %d wrong, %d crashed".format(benchmarked.duration, correct, wrong, crashed))
          count += 1
          count < 5
        } else {
          log("no more solutions, took %dms".format(benchmarked.duration))
          false
        }
      } catch {
        case e: Exception =>
          e.printStackTrace()
          log("next solution crashed: %s(%s)".format(e.getClass.getSimpleName, e.getMessage()))
          false
      }
    } while (continue)
    count
  }
}