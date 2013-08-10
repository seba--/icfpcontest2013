package solver.filter

import solver.Filter
import solver.FilterV._
import solver.ProblemSpec
import lang.Abstract._

class CompositeFilter(filters: List[Filter]) extends Filter {
	
	def init(spec: ProblemSpec) {
	  filters.foreach(_.init(spec))
	}
	
	def notifyNewData(data: Map[Long, Long]) {
	  filters.foreach(_.notifyNewData(data))
	}
	
	//returns the maximum result using shortcut-evaluation
	//do not replace foldLeft by FoldRight
	def filter(e : Exp): Int =
	  filters.foldLeft(OK)((curr, next) => max(curr, next.filter, e))
}