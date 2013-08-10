package solver

import org.scalatest.FunSuite
import lang.FlatAbstract._
import lang.FlatAbstract.Node._
import scala.collection.mutable.DoubleLinkedList
import solver.mutators.LinearMutator

class LinearMutatorTest extends FunSuite {
  test("mutate Node") {
    LinearMutator.init(new ProblemSpec("", 10, List(And, Or, Xor), null))
    val expected = List(Zero -> One,
                        One -> MainVar,
                        MainVar -> And,
                        And -> Or,
                        Or -> Xor)
    expected.foreach((s) => {
      LinearMutator.mutateNode(s._1) match {
        case Some(n) => assert(n == s._2)
        case None => fail
      }
    })
    
    LinearMutator.mutateNode(Xor) match {
      case Some(_) => fail
      case None => 
    }
  }
  
  test("mutate empty-Exp") {
    LinearMutator.init(new ProblemSpec("", 10, null, null))
    assert(LinearMutator.mutate(DoubleLinkedList()).get.equals(DoubleLinkedList(Zero)))
  }
  
  test("mutate at position") {
    LinearMutator.init(new ProblemSpec("", 10, List(Not), null))
    val expected = List((DoubleLinkedList(Zero), 0) -> DoubleLinkedList(One),
                        (DoubleLinkedList(One), 0) -> DoubleLinkedList(Not, Zero),
                        (DoubleLinkedList(Not, Zero), 1) -> DoubleLinkedList(Not, One),
                        (DoubleLinkedList(Not, One), 1) -> DoubleLinkedList(Not, Not, Zero))
    val exceptionExpected = List((DoubleLinkedList(Not, Zero), 0),
                                 (DoubleLinkedList(Not, One), 0),
                                 (DoubleLinkedList(Not, Not, Zero), 0),
                                 (DoubleLinkedList(Not, Not, One), 0),
                                 (DoubleLinkedList(Not, Not, Zero), 1),
                                 (DoubleLinkedList(Not, Not, One), 1))
    expected.foreach(s => assert(LinearMutator.mutateAtPosition(s._1._1, s._1._2).equals(s._2)))
  }
  
  test("generate possible Programs") {
    LinearMutator.init(new ProblemSpec("", 10, List(Not), null))
    var actualProgram = DoubleLinkedList[Node]()
    for (_ <- 0 until 100) {
      actualProgram = LinearMutator.mutate(actualProgram).get
      println(actualProgram)
    }
  }
}