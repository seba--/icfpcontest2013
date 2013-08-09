package lang

import org.scalatest.FunSuite

import lang.Abstract._
import lang.Semantics.eval

class SomeTests extends FunSuite{

  def testEval(p: Prg, in: Semantics.Value, out: Semantics.Value): Unit = {
    val result = eval(p)(in)
    assert(result === out, "Input: " + in + ", output: " + out)
  }
    
  def testEval(p: Prg, seq: Seq[(Semantics.Value,Semantics.Value)]): Unit =
    for (s <- seq)
      testEval(p, s._1, s._2)

  test("4n75sUkFvpQxpD3zhSTQg7mE") {
    val prog = Concrete.parse("(lambda (x_7948) (if0 (xor (or x_7948 1) x_7948) x_7948 1))")._1
    
    val expected = List(
      0x00000000000000L -> 0x0000000000000001L,
      0x00000000000001L -> 0x0000000000000001L,
      0x00000000000002L -> 0x0000000000000001L,
      0x00000000000003L -> 0x0000000000000003L,
      0x00000000000004L -> 0x0000000000000001L,
      0x00000000000005L -> 0x0000000000000005L,
      0x00000000000006L -> 0x0000000000000001L,
      0x00000000000007L -> 0x0000000000000007L
    )
    
    testEval(prog, expected)
  }
} 