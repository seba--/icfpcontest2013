package solver.filter

import solver.Filter
import solver.ProblemSpec
import lang.Abstract._

class CompositeFilter(filters: List[Filter]) extends Filter {
	
	def init(spec: ProblemSpec) {
	  filters.foreach(_.init(spec))
	}
	
	def notifyNewData(data: Map[Long, Long]) {
	  filters.foreach(_.notifyNewData(data))
	}
	
	//returns true if all contained filters return true, false otherwise
	def filter(e : Exp): Int = 
	  filters.foldRight(0)((f,y) => Math.max(f.filter(e),y))
}