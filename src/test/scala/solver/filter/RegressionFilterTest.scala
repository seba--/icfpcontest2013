package solver.filter

import org.scalatest.FunSuite
import solver.Filter

/**
 * Created with IntelliJ IDEA.
 * User: seba
 * Date: 10.08.13
 * Time: 23:31
 * To change this template use File | Settings | File Templates.
 */

import lang.Abstract.Operator._

class RegressionFilterTest extends FilterTest {

  val filters = List(
    new SizeFilter,
    new ValidFoldFilter,
    new ConstantFoldingFilter,
    new ShortcutShiftFilter,
    new IdentityOpFilter,
    new TFoldConditionFilter,
    new EvalFilter)
  override def createFilter() : Filter = new CompositeFilter(filters)

  test("test1") {
    initFilterWithOps(TFold, Xor, Shl1)
    assertAccepts("(lambda (x_4789) (fold x_4789 0 (lambda (x_4789 x_4790) (xor (shl1 x_4790) x_4789))))")
  }

  test("test2") {
    initFilterWithOps(TFold, Plus)
    assertAccepts("(lambda (x_879) (fold x_879 0 (lambda (x_879 x_880) (plus x_880 x_879))))")
  }
}
