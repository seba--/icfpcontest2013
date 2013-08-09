package solver

import lang.FlatAbstract._

final case class ProblemSpec(size : Int, operators: List[Node], data: collection.mutable.Map[Long, Long])
