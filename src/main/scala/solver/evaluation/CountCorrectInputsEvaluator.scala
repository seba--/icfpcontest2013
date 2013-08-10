package solver
package evaluation

import model.TrainingProblem
import lang.Semantics
import datacollection.BotApp._
import lang.Abstract.Exp

class EvaluatedSolution(nextSolutionBenchmark: Benchmarked[Exp], problem: TrainingProblem) {
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
  def apply(program: Exp, problem: TrainingProblem): EvaluationResultCounter = {
    problem.evaluationResultsAsLong.foldLeft(EvaluationResultCounter(0, 0, 0)) {
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

class CountCorrectInputsEvaluator(problems: Iterable[TrainingProblem]) extends SolverEvaluator {
  override def evaluate(solver: Solver) {
    problems.map(problem => problem -> evaluate(problem, solver))
  }

  def evaluate(problem: TrainingProblem, solver: Solver) {
    log("starting init for problem: %s, program size: %d, operators: %s".format(problem.id, problem.size, problem.operators))
    log("init took %dms".format(Benchmarked(solver.init(ProblemSpec(problem))).duration))
    var continue = true;
    do {
      val benchmarked = Benchmarked(solver.nextSolution())
      continue = benchmarked.value.isDefined
      if (continue) {
        val evaled = new EvaluatedSolution(benchmarked.map { _.get }, problem)
        val EvaluationResultCounter(correct, wrong, crashed) = evaled.counts
        log("solution took %dms, results: %d correct, %d wrong, %d crashed".format(benchmarked.duration, correct, wrong, crashed))
      } else {
        log("no more solutions, took %dms".format(benchmarked.duration))
      }
    } while (continue)
  }
}