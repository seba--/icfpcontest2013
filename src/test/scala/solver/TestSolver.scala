//package solver
//
//import lang.Semantics
//import lang.FlatAbstract._
//import org.scalatest.FunSuite
//import solver.solvers.BruteForceSizeFilteredSolver
//import datacollection.TrainingProblemStore
//import java.io.File
//import lang.Concrete
//import model.Problem
//import model.TrainingProblem
//
//case class TestSolver(solver: Solver) {
//  
//  def testSolve(spec: ProblemSpec) {
//    solver.init(spec)
//    
//    var sol: Option[Exp] = None
//    do {
//      sol = solver.nextSolution
//      
//      spec.data.foreach { case (input,output) => 
//        val result = Semantics.eval(sol.get)(input)
//        if (result != output)
//          println(s"Solver failed for problem ${spec.id} on input $input, expected: $output, but was: " + Semantics.toString(result))
//          println(s"Proposed solution was $sol")
//      }
//    } while (sol.isDefined)
//  }
//}
//
//class BruteForceSizeFilteredSolverTest extends FunSuite {
//  val store = TrainingProblemStore(new File("problems/train3"))
//  
//  test("4n75sUkFvpQxpD3zhSTQg7mE") {
//    testProblem(store.read("4n75sUkFvpQxpD3zhSTQg7mE"))
//  }
//  
//  test("BruteForceSizeFilteredSolver") {
//    store.allProblems.foreach { problem =>
//      testProblem(problem)
//    }
//  }
//  
//  def testProblem(problem: TrainingProblem) {
//    val ops = problem.operators.map(getNode)
//    val data = problem.evaluationResults.map({case (x,y) => (Semantics.fromString(x), Semantics.fromString(y))})
//    val spec = ProblemSpec(problem.id, problem.size, ops, collection.mutable.Map() ++ data)
//      
//    println("TEST " + problem.id)
//      
//    val tester = TestSolver(new BruteForceSizeFilteredSolver)
//    tester.testSolve(spec)
//      
//    println("SOLVED " + problem.id)
//  }
//}
