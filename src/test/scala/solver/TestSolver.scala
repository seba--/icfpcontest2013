package solver

import lang.Semantics
import lang.FlatAbstract._
import org.scalatest.FunSuite
import solver.solvers.BruteForceSizeFilteredSolver
import datacollection.TrainingProblemStore
import java.io.File
import lang.Concrete

case class TestSolver(solver: Solver) {
  
  def testSolve(spec: ProblemSpec) {
    solver.init(spec)
    
    var sol: Option[Exp] = None
    do {
      sol = solver.nextSolution
      
      spec.data.foreach { case (input,output) => 
        val result = Semantics.eval(sol.get)(input)
        if (result != output)
          println(s"Solver failed on input $input, expected: $output, but was: " + Semantics.toString(result))
          println(s"Proposed solution was $sol")
      }
    } while (sol.isDefined)
  }
}

object BruteForceSizeFilteredSolverTest extends FunSuite {
  test("BruteForceSizeFilteredSolver") {
    val store = TrainingProblemStore(new File("problems/trainWith0to255eval"))
    store.allProblems.foreach { problem =>
      val ops = problem.operators.map(getNode)
      val data = problem.evaluationResults.map({case (x,y) => (Semantics.fromString(x), Semantics.fromString(y))})
      val spec = ProblemSpec(problem.size, ops, collection.mutable.Map() ++ data)
      
      val tester = TestSolver(new BruteForceSizeFilteredSolver)
      tester.testSolve(spec)
    }
  }
}
