package solver

import org.scalatest.FunSuite

import lang.Abstract.Exp
import solver.FilterV.STEP_INTO
import solver.FilterV.STEP_OVER
import solver.FilterV.max_
import solver.FilterV.max

class MaxFilterTest extends FunSuite {
  val MAX_VALUE = STEP_OVER
  val AVG_VALUE = STEP_INTO

  def forbiddenFun(e: Exp): Int = {
    println("missed")
    fail
  }

  def maxFun(e: Exp): Int = {
    MAX_VALUE
  }

  def avgFun(e: Exp): Int = {
    AVG_VALUE
  }

  test("take the first") {
    assert(max(MAX_VALUE, forbiddenFun, null) == STEP_OVER)
  }

  test("calculate max") {
    assert(max(AVG_VALUE, maxFun, null) == STEP_OVER)
  }

  test("take the first 2") {
    assert(max_(maxFun, forbiddenFun, null) == STEP_OVER)
  }

  test("calculate max 2") {
    assert(max_(avgFun, maxFun, null) == STEP_OVER)
  }

  test("fold take the first") {
    val seq: List[(Exp) => Int] = List(maxFun, forbiddenFun, forbiddenFun)
    assert(seq.foldLeft(0)((curr, next) => max(curr, next, null)) == STEP_OVER)
  }

  test("fold calculate max") {
    val seq: List[(Exp) => Int] = List(avgFun, maxFun, forbiddenFun)
    assert(seq.foldLeft(0)((curr, next) => max(curr, next, null)) == STEP_OVER)
  }
}