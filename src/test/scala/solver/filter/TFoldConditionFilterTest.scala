package solver.filter

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import solver.Filter
import solver.ProblemSpec
import solver.ProblemSpec
import lang.Abstract._
import lang.Abstract.Operator._
import scala.collection.mutable.Map

class TFoldConditionFilterTest extends FilterTest {
  override def createFilter() : Filter = new TFoldConditionFilter
  
  def specWithOps(ops : Operator*) : ProblemSpec = {
    ProblemSpec("testId", 42, ops.toList, Map[Long,Long]())
  }
  
  test("no tfold should be accepted") {
    initFilter(specWithOps(And, Or))
    assertAccepts("(lambda (x) x)")
  }
  
  test("correct tfold should be accepted") {
    initFilter(specWithOps(TFold))
    assertAccepts("(lambda (x) (fold x 0 (lambda (x y) x)))")
  }
  
  test("no tfold but required should be denied") {
    initFilter(specWithOps(TFold))
    assertDenies("(lambda (x) (or x 1))")
  }
  
  test("tfold with main var in fold body should  be denied") {
    initFilter(specWithOps(TFold))
    assertDenies("(lambda (x) (fold x 0 (lambda (a b) x)))")
  }
  
  test("tfold with init other than 0 should be denied") {
    initFilter(specWithOps(TFold))
    assertDenies("(lambda (x) (fold x 1 (lambda (a b) 0)))")
  }

}