package solver

import lang.Semantics
import lang.Abstract._
import org.scalatest.FunSuite
import solver.solvers.BruteForceSizeFilteredSolver
import datacollection.TrainingProblemStore
import java.io.File
import client.api.Problem
import solver.solvers.PartialSolver

class BruteForceSizeFilteredSolverTest extends FunSuite with TestUtils {
  val store = TrainingProblemStore(new File("problems/train3"))

  implicit val solver = new BruteForceSizeFilteredSolver

  test("1kqy5F4l3GIrDzZL73e4ZLrO") {
    testProblem(store.read("1kqy5F4l3GIrDzZL73e4ZLrO").asProblem)
  }

  //  test("BruteForceSizeFilteredSolver") {
  //    store.allProblems.foreach {
  //      problem =>
  //        testProblem(client.api.Problem(problem))
  //    }
  //  }

}

