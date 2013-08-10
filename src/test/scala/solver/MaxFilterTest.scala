package solver

import org.scalatest._
import lang.Abstract.Exp

class MaxFilterTest extends FunSuite {

  test("take the first") {
    val v = 2
    def f(e:Exp) : Int = {
      println("missed")
      fail
    }
    println(solver.FilterV.max_(v, f, null))
  }
  
  test("calculate max") {
    val v = 1
    def f(e:Exp) : Int = {
      println("check")
      2
    }
    assert(solver.FilterV.max_(v, f, null) == solver.FilterV.STEP_OVER)
  }
  
  test("take the first 2") {
    def v(e:Exp) = {2}
    def f(e:Exp) : Int = {
      println("missed")
      fail
    }
    println(solver.FilterV.max(v, f, null))
  }
  
  test("calculate max 2") {
    def v(e:Exp) = {2}
    def f(e:Exp) : Int = {
      println("check")
      2
    }
    assert(solver.FilterV.max(v, f, null) == solver.FilterV.STEP_OVER)
  }
}