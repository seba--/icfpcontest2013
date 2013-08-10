package solver.filter

import scala.collection.Seq
import java.util.concurrent.atomic.AtomicReference
import org.scalatest.Reporter
import solver.Filter
import scala.collection.immutable.Map
import scala.reflect.Manifest
import scala.runtime.BoxedUnit

class ConstantFoldingFilterTest extends FilterTest {
  
  override def createFilter() : Filter = new ConstantFoldingFilter

  test("denies if with constant condition false") {
    assertDenies("(lambda (x) (if0 0 x x))")
  }
  
  test("denies if with constant, complex condition") {
    assertDenies("(lambda (x) (if0 (plus 0 1) x x))")
  }
  
  test("denies fold with contant body") {
    assertDenies("(lambda (x) (fold x 0 (lambda (x y) 1)))")
  }
  
  test("accepts if with variable condition") {
    assertAccepts("(lambda (x) (if0 x x x))")
  }
  
  test("accepts constant program") {
    assertAccepts("(lambda (x) (shr4 0))")
  }
  
}