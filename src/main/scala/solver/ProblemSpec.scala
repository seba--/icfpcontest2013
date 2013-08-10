package solver

import lang.Abstract._
import model.TrainingProblem
import lang.Concrete

/**
 * If you need more elaborate metadata, add it to this problem specification class as public fields that
 * all components can make use of. This is to avoid duplicate CPU and memory effort.
 */
case class ProblemSpec(
    id: String, 
    size : Int, 
    operators: List[Operator], 
    var data: Map[Long, Long]) {
  // maximal number an operator can be used in the solution
  // val maxOperatorUsage: Map[Node, Int]
}

object ProblemSpec {
  def apply(problem: TrainingProblem) = new ProblemSpec(problem.id, problem.size, mapOperators(problem.operators), problem.evaluationResultsAsLong)
  private def mapOperators(ops: List[String]) : List[Operator] = ops.map(Concrete.tryParseOperator(_).get)
}
