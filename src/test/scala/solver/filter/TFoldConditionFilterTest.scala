package solver.filter

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import solver.Filter
import solver.ProblemSpec
import solver.ProblemSpec
import lang.Abstract._
import lang.Abstract.Operator._

class TFoldConditionFilterTest extends FilterTest {

  override def createFilter(): Filter = new TFoldConditionFilter

  test("no tfold should be accepted") {
    initFilterWithOps(And, Or)
    assertAccepts("(lambda (x) x)")
  }

  test("fold is not accepted as first node if no tnode is given") {
    initFilterWithOps(And, Or)
    assertDenies("(lambda (x) (fold x 0 (lambda (x y) x)))")
  }

  test("fold is accepted as first node if no tnode is given because 1 != 0") {
    initFilterWithOps(And, Or)
    assertAccepts("(lambda (x) (fold x 1 (lambda (x y) x)))")
  }

  test("correct tfold should be accepted") {
    initFilterWithOps(TFold)
    assertAccepts("(lambda (x) (fold x 0 (lambda (x y) x)))")
  }

  test("no tfold but required should be denied") {
    initFilterWithOps(TFold)
    assertDenies("(lambda (x) (or x 1))")
  }

  test("tfold with main var in fold body should  be denied") {
    initFilterWithOps(TFold)
    assertDenies("(lambda (x) (fold x 0 (lambda (a b) x)))")
  }

  test("tfold with init other than 0 should be denied") {
    initFilterWithOps(TFold)
    assertDenies("(lambda (x) (fold x 1 (lambda (a b) 0)))")
  }
}
