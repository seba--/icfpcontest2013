package lang

import org.scalatest.FunSuite
import json.TrainingProblemStore

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
    for (id <- TrainingProblemStore.ids) {
      val prob = TrainingProblemStore.read(id)
      try {
        parse(prob.challenge)
      } catch {
        case ParseException(msg, str) =>
          println("FAILED " + id)
          assert(false, msg + ", was: " + str)
      }
      println("OK " + id)
    }
  }
}