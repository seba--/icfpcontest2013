package solver

import lang.Semantics
import lang.Abstract._
import org.scalatest.FunSuite
import solver.solvers.BruteForceSizeFilteredSolver
import datacollection.TrainingProblemStore
import java.io.File
import client.api.Problem
import solver.solvers.PartialSolver

trait TestUtils {

  def testProblem(problem: Problem)(implicit solver: Solver) {
    println("TEST " + problem.id)

    testSolve(problem)

    println("SOLVED " + problem.id)
  }

  def testSolve(spec: Problem)(implicit solver: Solver): Int = {
    solver.init(spec)

    var sol: Option[Exp] = None
    var solCount = 0
    //do {
    sol = solver.nextSolution

    if (sol.isDefined) {
      println("Found solution " + sol.get)
      solCount += 1
      spec.evaluationResults.get.foreach {
        case (input, output) =>
          val result = Semantics.eval(sol.get)(input)
          if (result != output) {
            println(s"Solver failed for problem ${spec.id} on input $input, expected: $output, but was: " + Semantics.toString(result))
            println(s"Proposed solution was $sol")
          }
      }
    }
    //} while (!sol.isDefined)
    return solCount
  }

}
