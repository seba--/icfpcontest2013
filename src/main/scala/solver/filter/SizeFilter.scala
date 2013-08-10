package solver.filter

import solver.Filter
import solver.FilterV
import scala.collection.immutable.Map
import solver.ProblemSpec
import lang.Abstract._
import lang.Metadata._

class SizeFilter extends Filter {
  var spec: ProblemSpec = null
  
  def init(spec: ProblemSpec) {
    this.spec = spec
  }
  
  
  def notifyNewData(data: Map[Long, Long]) {
    // ignore
  }

  // keep expressions with valid size
  def filter(e: Exp): Int =
    if (size(e) <= spec.size) FilterV.OK else FilterV.STEP_OVER
}