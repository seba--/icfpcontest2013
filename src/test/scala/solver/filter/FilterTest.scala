package solver.filter

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import solver.Filter
import lang.Concrete.parse
import solver.ProblemSpec

abstract class FilterTest extends FunSuite with BeforeAndAfter {

  var filterUnderTest : Filter = _
  
  before {
    filterUnderTest = createFilter
  }
  
  def createFilter() : Filter
  
  def initFilter(spec: ProblemSpec) {
    filterUnderTest.init(spec)
  }
  
  def assertAccepts(program: String) {
    assert(filterUnderTest.filter(parse(program)))
  }
  
  def assertDenies(program: String) {
    assert(!filterUnderTest.filter(parse(program)))
  }
}