package solver.filter

import scala.collection.Seq
import java.util.concurrent.atomic.AtomicReference
import org.scalatest.Reporter
import solver.Filter
import scala.collection.immutable.Map
import scala.reflect.Manifest
import scala.runtime.BoxedUnit
import java.lang.reflect.Method

class DoubleFoldFilterTest extends FilterTest {

  override def createFilter() : Filter = new DoubleFoldFilter
  
  test("accepts program without fold") {
    assertAccepts("(lambda (x) (plus x (shr4 x)))")
  }
  
  test("denies nested folds") {
    assertDenies("(lambda (x) (fold x 0 (lambda (y z) (fold y 1 (lambda (a b) (plus a b))))))")
  }
  
  test("denies sibling folds") {
    assertDenies("(lambda (x) (and (fold x x (lambda (x y) x)) (fold x x (lambda (x y) x))))")
  }
}