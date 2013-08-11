package lang

import org.scalatest.FunSuite

import datacollection.TrainingProblemStore
import lang.Abstract.MainVar
import lang.Concrete.ParseException
import lang.Concrete.parse

class ParserSuite extends FunSuite {

  import lang.Abstract._
  import lang.Concrete.parse

  test("id function") {
    val res = parse("(lambda (x) x)")
    assert(res === MainVar)
  }


  test("pretty printing") {
    val s = "(lambda (main_var) (fold main_var 0 (lambda (fold_next fold_acc) (plus fold_next fold_acc))))"
    parseAndAssertSerialization("main_var", s)
  }

  // unknown parsing

  test("simplest partial programm") {
    parseAndAssertSerialization("(lambda (x) ?)")
  }

  test("if0") {
    parseAndAssertSerialization("(lambda (x) (if0 ? ? ?))")
  }

  test("unknown parts in a fold") {
    parseAndAssertSerialization("(lambda (x) (fold ? ? (lambda (fold_next fold_acc) ?)))")
  }

  // op1

  test("not") {
    parseAndAssertSerialization("(lambda (x) (not ?))")
  }

  test("shl1") {
    parseAndAssertSerialization("(lambda (x) (shl1 ?))")
  }

  test("shr1") {
    parseAndAssertSerialization("(lambda (x) (shr1 ?))")
  }

  test("shr4") {
    parseAndAssertSerialization("(lambda (x) (shr4 ?))")
  }

  test("shr16") {
    parseAndAssertSerialization("(lambda (x) (shr16 ?))")
  }

  // op2

  test("and") {
    parseAndAssertSerialization("(lambda (x) (and ? ?))")
  }

  test("or") {
    parseAndAssertSerialization("(lambda (x) (or ? ?))")
  }

  test("xor") {
    parseAndAssertSerialization("(lambda (x) (xor ? ?))")
  }

  test("plus") {
    parseAndAssertSerialization("(lambda (x) (plus ? ?))")
  }

  // other unknowns

  test("deep nested unknown") {
    parseAndAssertSerialization("(lambda (x) (or 1 (and 0 (xor 1 (plus 0 ?)))))")
  }

  def parseAndAssertSerialization(p: String) {
    parseAndAssertSerialization("x", p)
  }

  def parseAndAssertSerialization(mainvar : String, p: String) {
    val res = parse(p)
    val serial = "(lambda ("+mainvar+") " + res.toString() + ")"
    assert(p === serial)
  }
}

class TrainingDataParserSuite extends FunSuite {

  import lang.Concrete.parse
  import lang.Concrete.ParseException

  test("training data") {
    TrainingProblemStore.default.allProblems.foreach { prob =>
      try {
        parse(prob.challenge.get)
      } catch {
        case ParseException(msg, str) =>
          println("FAILED " + prob.id)
          assert(false, msg + ", was: " + str)
      }
      println("OK " + prob.id)
    }
  }
}
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
