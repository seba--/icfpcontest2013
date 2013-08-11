package solver.filter

import solver.Filter

class BinaryComparisonFilterTest extends FilterTest {

  override def createFilter() : Filter = new BinaryComparisonFilter
  
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
}