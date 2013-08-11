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
	//do not replace 'foldLeft' by 'FoldRight'
	//use 'or' instead of 'max' for optimization if:
	// - at most one Filter can return both STEP_INTO and STEP_OVER
	// - the first (half of the) Filters can only return STEP_OVER (or OK)
	// - the last (half of the) Filters can only return STEP_INTO (or OK)
	//if using 'or' then sort the groups in ascending runtime to get an optimal overall runtime
	def filter(e : Exp): Int =
	  filters.foldLeft(OK)((curr, next) => max(curr, next.filter, e))
}