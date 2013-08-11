package solver

import lang.Abstract._
import lang.Concrete
import client.api.Problem

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
  def apply(problem: Problem) = new ProblemSpec(problem.id, problem.size, problem.operators, problem.evaluationResults.getOrElse(Map()))
}
