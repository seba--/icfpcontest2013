package solver

import lang.Semantics
import lang.Abstract._
import org.scalatest.FunSuite
import solver.solvers.BruteForceSizeFilteredSolver
import datacollection.TrainingProblemStore
import java.io.File
import client.api.Problem
import solver.solvers.PartialSolver

class PartialSolverTest extends FunSuite with TestUtils {

  val store = TrainingProblemStore(new File("problems/train3"))

  implicit val solver = new PartialSolver

  // 03dMrc1C9TLG8lNLiFFmYsyT // (size 25)
  test("1kqy5F4l3GIrDzZL73e4ZLrO") {
    testProblem(client.api.Problem(store.read("1kqy5F4l3GIrDzZL73e4ZLrO")))
  }

}
