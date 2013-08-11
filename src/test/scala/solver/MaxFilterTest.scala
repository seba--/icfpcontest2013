package solver

import org.scalatest.FunSuite

import lang.Abstract.Exp
import solver.FilterV.OK
import solver.FilterV.STEP_INTO
import solver.FilterV.STEP_OVER
import solver.FilterV.max
import solver.FilterV.max_
import solver.FilterV.or
import solver.FilterV.or_

class MaxFilterTest extends FunSuite {
  val MAX_VALUE = STEP_OVER
  val AVG_VALUE = STEP_INTO
  val MIN_VALUE = OK

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

  def minFun(e: Exp): Int = {
    MIN_VALUE
  }

  test("max: take the first") {
    assert(max(MAX_VALUE, forbiddenFun, null) == STEP_OVER)
  }

  test("max: apply math. max") {
    assert(max(AVG_VALUE, maxFun, null) == STEP_OVER)
  }

  test("max_: take the first") {
    assert(max_(maxFun, forbiddenFun, null) == STEP_OVER)
  }

  test("max_: apply math. max") {
    assert(max_(avgFun, maxFun, null) == STEP_OVER)
  }

  test("max: fold take the first") {
    val seq: List[(Exp) => Int] = List(maxFun, forbiddenFun, forbiddenFun)
    assert(seq.foldLeft(0)((curr, next) => max(curr, next, null)) == STEP_OVER)
  }

  test("max: fold apply math. max") {
    val seq: List[(Exp) => Int] = List(avgFun, maxFun, forbiddenFun)
    assert(seq.foldLeft(0)((curr, next) => max(curr, next, null)) == STEP_OVER)
  }

  test("or: take the first (like max)") {
    assert(or(MAX_VALUE, forbiddenFun, null) == STEP_OVER)
  }

  test("or_: take the first (like max)") {
    assert(or_(maxFun, forbiddenFun, null) == STEP_OVER)
  }

  test("or: take the first") {
    assert(or(AVG_VALUE, forbiddenFun, null) == STEP_INTO)
  }

  test("or: apply math. max") {
    assert(or(MIN_VALUE, avgFun, null) == STEP_INTO)
  }

  test("or_: take the first") {
    assert(or_(avgFun, forbiddenFun, null) == STEP_INTO)
  }

  test("or_: apply math. max") {
    assert(or_(minFun, avgFun, null) == STEP_INTO)
  }

  test("or: fold take the first (like max)") {
    val seq: List[(Exp) => Int] = List(maxFun, forbiddenFun, forbiddenFun)
    assert(seq.foldLeft(0)((curr, next) => or(curr, next, null)) == STEP_OVER)
  }

  test("or: fold take the first") {
    val seq: List[(Exp) => Int] = List(avgFun, forbiddenFun, forbiddenFun)
    assert(seq.foldLeft(0)((curr, next) => or(curr, next, null)) == STEP_INTO)
  }

  test("or: fold apply math. max") {
    val seq: List[(Exp) => Int] = List(minFun, avgFun, forbiddenFun)
    assert(seq.foldLeft(0)((curr, next) => or(curr, next, null)) == STEP_INTO)
  }
}