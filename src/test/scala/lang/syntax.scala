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


class MakeFlattTester extends FunSuite {
  import lang.FlatAbstract._
  
  test("flatten 4n75sUkFvpQxpD3zhSTQg7mE") {
    val prog = lang.Concrete.parse("(lambda (x_7948) (if0 (xor (or x_7948 1) x_7948) x_7948 1))".replace("x_7948", "main_var"))._1
    
    val flat = makeFlat(prog)
    
    val struct = makeStructuralPrg(flat)
    
    assert(prog === struct)
  }

  test("flatten 6cLtWMudZs1CMhDOLqYaI3U2") {
    val prog = lang.Concrete.parse("(lambda (x_50948) (fold (xor (or (if0 (and (not (or x_50948 x_50948)) x_50948) 0 1) 1) x_50948) 0 (lambda (x_50949 x_50950) (if0 x_50950 x_50949 x_50950))))".replace("x_50948", "main_var").replace("x_50949", "fold_next").replace("x_50950", "fold_acc"))._1
    
    val flat = makeFlat(prog)
    
    val struct = makeStructuralPrg(flat)
    
    assert(prog === struct)
  }
}

