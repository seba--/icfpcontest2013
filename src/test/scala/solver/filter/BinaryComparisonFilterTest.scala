package solver.filter

import lang.Abstract.Box
import lang.Abstract.Exp
import lang.Abstract.Operator
import lang.Abstract.Operator.And
import lang.Abstract.Operator.If0
import lang.Abstract.Operator.Not
import lang.Abstract.Operator.Or
import lang.Abstract.Operator.Plus
import lang.Abstract.Operator.Shl1
import lang.Abstract.Operator.Shr1
import lang.Abstract.Operator.Shr16
import lang.Abstract.Operator.Shr4
import lang.Abstract.Operator.Xor
import lang.Metadata.size
import solver.Filter
import solver.ProblemSpec
import solver.mutators.LinearMutator

class BinaryComparisonFilterTest extends FilterTest {

  override def createFilter(): Filter = new BinaryComparisonFilter

  test("acccepts programs without binary expressions") {
    assertAccepts("(lambda (x) x)")
    assertAccepts("(lambda (x) 0)")
    assertAccepts("(lambda (x) 1)")
    assertAccepts("(lambda (x) (not x))")
    assertAccepts("(lambda (x) (not 1))")
    assertAccepts("(lambda (x) (not 0))")
    assertAccepts("(lambda (x) (shr16 x))")
    assertAccepts("(lambda (x) (if0 0 0 0))")
    assertAccepts("(lambda (x) (fold x 1 (lambda(a b) (shl1 b))))")
  }

  test("accepts programs with BApps if the first argument is 'greater' or 'equal' " +
    "as defined in solver.filter.BinaryComparisonFilter.compare") {
    assertAccepts("(lambda (x) (or 0 0))")
    assertAccepts("(lambda (x) (or 1 1))")
    assertAccepts("(lambda (x) (or x x))")
    assertAccepts("(lambda (x) (or (not x) (not x)))")
    assertAccepts("(lambda (x) (or 1 0))")
    assertAccepts("(lambda (x) (or x 0))")
    assertAccepts("(lambda (x) (or (not x) 0))")
    assertAccepts("(lambda (x) (or (not x) 1))")
    assertAccepts("(lambda (x) (or (not x) x))")
    assertAccepts("(lambda (x) (or (if0 0 0 0) (fold 0 0 (lambda (a b) 0))))")
  }

  test("denies programs with BApps if the first argument is 'less'") {
    assertDenies("(lambda (x) (or 0 1 ))")
    assertDenies("(lambda (x) (or 0 x))")
    assertDenies("(lambda (x) (or 0 (not x)))")
    assertDenies("(lambda (x) (or 1 (not x)))")
    assertDenies("(lambda (x) (or x (not x)))")
    assertDenies("(lambda (x) (or (and 1 0) (not x)))")
    assertDenies("(lambda (x) (or (fold 0 0 (lambda (a b) 0)) (if0 0 0 0)))")
    assertDenies("(lambda (x) (if0 (not 1) x (and 0 1)))")
  }

  test("advanced test: nested BApps are threated right: " +
    "right handling means first check if the nested BApps are acceptable themselves " +
    "and the arguments of the first are greater or equal to the ones of the second argument") {
    assertAccepts("(lambda (x) (or (or x 1) (or x 1)))")
    assertAccepts("(lambda (x) (or (or (not x) x) (or (not x) x)))")
    assertDenies("(lambda (x) (or (or x (not x)) (or x (not x))))")
    assertAccepts("(lambda (x) (or (not x) (and 1 0)))")
    assertDenies("(lambda (x) (or (or (not x) (xor 1 0)) (or (not x) x)))")
  }

  test("Exps mutate to strictly greater ones") {
    val filter = new BinaryComparisonFilter()
    LinearMutator.init(new ProblemSpec("", 0, List(And, Not), null))
    var small: Exp = Box()
    var big: Exp = Box()
    big = mutate(big).get
    var continue = true
    while (continue) {
      println(big)
      if (filter.compare(small, big) > 0)
        fail
      else {
        val newBig = mutate(big)
        if (newBig.isEmpty)
          continue = false
        else {
          big = newBig.get
          small = mutate(small).get
        }
      }
    }
  }

  def mutate(e: Exp): Option[Exp] = {
    val maxSize = 4
    val mutated = LinearMutator.stepInto(e)
    if (mutated.isEmpty || size(mutated.get) <= maxSize)
      mutated
    else
      mutateOver(mutated.get, maxSize)
  }

  def mutateOver(e: Exp, maxSize:Int): Option[Exp] = {
    val mutated = LinearMutator.stepOver(e)
    if (mutated.isEmpty || size(mutated.get) <= maxSize)
      mutated
    else
      mutateOver(mutated.get, maxSize)
  }
}