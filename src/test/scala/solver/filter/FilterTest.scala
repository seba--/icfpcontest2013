package solver.filter

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import solver.Filter
import lang.Concrete.parse
import solver.ProblemSpec
import lang.Abstract.Operator
import lang.Abstract.Operator._
import scala.collection.immutable.Map
import solver.FilterV

abstract class FilterTest extends FunSuite with BeforeAndAfter {

  var filterUnderTest : Filter = _
  
  before {
    filterUnderTest = createFilter
  }
  
  def createFilter() : Filter
  
  def initFilter(spec: ProblemSpec) {
    filterUnderTest.init(spec)
  }
  
  def initFilterWithOps(ops : Operator*) {
	  initFilter(ProblemSpec("testId", 42, ops.toList, Map[Long,Long]()))
  }
  
  def assertAccepts(program: String) {
    assert(filterUnderTest.filter(parse(program)) == FilterV.OK)
  }
  
  def assertDenies(program: String) {
    assert(filterUnderTest.filter(parse(program)) != FilterV.OK)
  }
}