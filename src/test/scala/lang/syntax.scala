package lang

import org.scalatest.FunSuite
import json.TrainingProblemStore

class ParserSuite extends FunSuite {
  
  import lang.Abstract._
  import lang.Concrete.parse
  
  test("id function") {
    val res = parse("(lambda (x) x)")
    assert(res === Left((Prg("x", Var("x")))))
  }
}

class TrainingDataParserSuite extends FunSuite {

  import lang.Concrete.parse
  
  test("training data") {
    for (id <- TrainingProblemStore.ids) {
      val prob = TrainingProblemStore.read(id)
      val result = parse(prob.challenge)
      assert(result.isLeft, result)
    }
  }
}