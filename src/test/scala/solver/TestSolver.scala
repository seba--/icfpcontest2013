package solver

import lang.Semantics
import lang.FlatAbstract._

case class TestSolver(solver: Solver, mutator: Mutator, filter: Filter, fitness: Fitness) {
  
  def testSolve(spec: ProblemSpec) {
    mutator.init(spec)
    filter.init(spec)
    fitness.init(spec)
    solver.init(spec, mutator, filter, fitness)
    
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