import lang.{Concrete, Metadata}
import org.scalatest.FunSuite

import lang.Abstract._

class SizeTest extends FunSuite {

  test("size1") {
    val s = Metadata.size(Concrete.parse("(lambda (main_var) (fold 0 0 (lambda (fold_next fold_acc) (plus 0 0))))"))
    assert (s === 8)
  }

}