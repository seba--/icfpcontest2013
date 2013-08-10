package solver

import lang.Semantics
import lang.Abstract._
import org.scalatest.FunSuite
import solver.solvers.BruteForceSizeFilteredSolver
import datacollection.TrainingProblemStore
import java.io.File
import lang.Concrete
import client.api.Problem

case class TestSolver(solver: Solver) {

  def testSolve(spec: Problem): Int = {
    solver.init(spec)

    var sol: Option[Exp] = None
    var solCount = 0
    do {
      sol = solver.nextSolution
      println("Found solution " + sol)

      if (sol.isDefined) {
        solCount += 1
        spec.evaluationResults.foreach {
          case (input, output) =>
            val result = Semantics.eval(sol.get)(input)
            if (result != output)
              println(s"Solver failed for problem ${spec.id} on input $input, expected: $output, but was: " + Semantics.toString(result))
            println(s"Proposed solution was $sol")
        }
      }
    } while (sol.isDefined)
    return solCount
  }
}

class BruteForceSizeFilteredSolverTest extends FunSuite {
  val store = TrainingProblemStore(new File("problems/trainWith0to255eval"))

  test("4n75sUkFvpQxpD3zhSTQg7mE") {
    testProblem(store.read("4n75sUkFvpQxpD3zhSTQg7mE").toClientProblem)
  }

  //  test("BruteForceSizeFilteredSolver") {
  //    store.allProblems.foreach { problem =>
  //      testProblem(problem)
  //    }
  //  }

  def testProblem(problem: Problem) {
    println("TEST " + problem.id)

    val tester = TestSolver(new BruteForceSizeFilteredSolver)
    tester.testSolve(problem)

    println("SOLVED " + problem.id)
  }
}
