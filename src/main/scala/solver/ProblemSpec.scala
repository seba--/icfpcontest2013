package solver

import lang.FlatAbstract._

/**
 * If you need more elaborate metadata, add it to this problem specification class as public fields that
 * all components can make use of. This is to avoid duplicate CPU and memory effort.
 */
case class ProblemSpec(
    id: String, 
    size : Int, 
    operators: List[Node], 
    data: collection.mutable.Map[Long, Long]) {
  // maximal number an operator can be used in the solution
  // val maxOperatorUsage: Map[Node, Int]
  
}
