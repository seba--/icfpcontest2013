package lang

import org.scalatest.FunSuite
import lang.Abstract._
import lang.Semantics.eval
import datacollection.TrainingProblemStore
import java.io.File
import scala.collection.immutable.SortedMap

class SomeTests extends FunSuite {

  def testEval(p: Exp, in: Semantics.Value, out: Semantics.Value): Unit = {
    val result = eval(p)(in)
    assert(result === out, "Input: " + in + ", output: " + out)
  }

  def testEval(p: Exp, seq: Iterable[(Semantics.Value, Semantics.Value)]): Unit =
    seq.foreach { s =>
      testEval(p, s._1, s._2)
    }

  test("4n75sUkFvpQxpD3zhSTQg7mE") {
    val prog = Concrete.parse("(lambda (x_7948) (if0 (xor (or x_7948 1) x_7948) x_7948 1))")

    val expected = List(
      0x00000000000000L -> 0x0000000000000001L,
      0x00000000000001L -> 0x0000000000000001L,
      0x00000000000002L -> 0x0000000000000001L,
      0x00000000000003L -> 0x0000000000000003L,
      0x00000000000004L -> 0x0000000000000001L,
      0x00000000000005L -> 0x0000000000000005L,
      0x00000000000006L -> 0x0000000000000001L,
      0x00000000000007L -> 0x0000000000000007L)

    testEval(prog, expected)
  }

  test("0 to 255 for all downloaded training problems") {
    val store = TrainingProblemStore(new File("problems/train3"))
    store.allProblems.foreach { problem =>
      val program = Concrete.parse(problem.challenge)
      problem.evaluationResults.foreach {
        case (input, output) =>
          val longInput = Semantics.fromString(input)
          val longOutput = Semantics.fromString(output)
          println("Test " + problem.id)
          val result = Semantics.eval(program)(longInput)
          assert(result === longOutput, s"Failed: ${problem.id} with input $input, expected: $output, but was: "+Semantics.toString(result))
          println(s"ok: ${problem.id} with input $input")
      }
    }
  }
} 