package solver

import org.scalatest.FunSuite

class SolverSuite extends FunSuite {

  import solver.old.SvenSolver
  import lang.Abstract._
  
  test("const 0") {
    val solver = new SvenSolver(1, List[Operator]())
    val solution = solver.solve(Map(0l -> 0l, 1l -> 0l, 2l -> 0l, 3l -> 0l))
    assert(solution == Zero())
  }
}