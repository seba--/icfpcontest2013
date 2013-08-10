package lang

import org.scalatest.FunSuite
import datacollection.TrainingProblemStore
import java.io.File

class ParserSuite extends FunSuite {

  import lang.Abstract._
  import lang.Concrete.parse

  test("id function") {
    val res = parse("(lambda (x) x)")
    assert(res === MainVar())
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


//class MakeFlattTester extends FunSuite {
//  import lang.Abstract._
//  
//  test("flatten 4n75sUkFvpQxpD3zhSTQg7mE") {
//    val prog = lang.Concrete.parse("(lambda (x_7948) (if0 (xor (or x_7948 1) x_7948) x_7948 1))".replace("x_7948", "main_var"))
//    
//    val struct = makeStructuralPrg(prog)    
//    val flat = makeFlat(struct)
//    assert(prog === flat)
//  }
//
//  test("flatten 6cLtWMudZs1CMhDOLqYaI3U2") {
//    val prog = lang.Concrete.parse("(lambda (x_50948) (fold (xor (or (if0 (and (not (or x_50948 x_50948)) x_50948) 0 1) 1) x_50948) 0 (lambda (x_50949 x_50950) (if0 x_50950 x_50949 x_50950))))".replace("x_50948", "main_var").replace("x_50949", "fold_next").replace("x_50950", "fold_acc"))
//    
//    val struct = makeStructuralPrg(prog)    
//    val flat = makeFlat(struct)
//    assert(prog === flat)
//  }
//
//  test("training data") {
//    TrainingProblemStore.default.allProblems.foreach { prob =>
//      val prog = lang.Concrete.parse(prob.challenge)
//      val struct = makeStructuralPrg(prog)    
//      val flat = makeFlat(struct)
//      assert(prog === flat)
//    }
//  }
//}
