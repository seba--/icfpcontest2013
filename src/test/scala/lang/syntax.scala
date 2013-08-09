package lang

import org.scalatest.FunSuite
import datacollection.TrainingProblemStore
import java.io.File

class ParserSuite extends FunSuite {

  import lang.Abstract._
  import lang.Concrete.parse

  test("id function") {
    val res = parse("(lambda (x) x)")
    assert(res === ((Prg("x", Var("x")), "")))
  }
}

class TrainingDataParserSuite extends FunSuite {

  import lang.Concrete.parse
  import lang.Concrete.ParseException

  test("training data") {
    TrainingProblemStore.default.allProblems.foreach { prob =>
      try {
        parse(prob.challenge)
      } catch {
        case ParseException(msg, str) =>
          println("FAILED " + prob.id)
          assert(false, msg + ", was: " + str)
      }
      println("OK " + prob.id)
    }
  }
}