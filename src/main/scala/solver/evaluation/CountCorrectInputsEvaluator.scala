package solver
package evaluation

import model.TrainingProblem
import lang.Semantics

class CountCorrectInputsEvaluator(problems: Iterable[TrainingProblem]) extends SolverEvaluator {
  override def evaluate(solver: Solver) {
    problems.map(problem => problem -> evaluate(problem, solver))
  }

  def evaluate(problem: TrainingProblem, solver: Solver) {
    println("starting init...")
    println("init took %dms".format(benchmark(solver.init(ProblemSpec(problem)))._2))
    var continue = true;
    do {
      val (maybeNextSolution, duration) = benchmark(solver.nextSolution())
      continue = maybeNextSolution match {
        case Some(nextSolution) =>
          val correctResults = problem.evaluationResultsAsLong.count {
            case (in, out) =>
              Semantics.eval(nextSolution)(in) == out
          }
          println("solution took %dms, %d/%d correct results".format(duration, correctResults, problem.evaluationResults.size))
          true
        case None =>
          println("no more solutions, took %dms".format(duration))
          false
      }
    } while (continue)
  }
}