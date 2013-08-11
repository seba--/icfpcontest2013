package solver

import org.scalatest.FunSuite
import lang.Abstract._
import lang.Abstract.Operator._
import scala.collection.mutable.DoubleLinkedList
import solver.mutators.LinearMutator
import lang.Metadata

class LinearMutatorTest1 extends FunSuite {
  test("gernerate Not-Programs") {
    val mutator = new LinearMutator
    mutator.init(new ProblemSpec("", 0, List(Not, And), null))
    var actualProgram : Exp = Box()
    for (_ <- 0 to 100) {
      actualProgram = mutator.stepInto(actualProgram).get
      // should be only "not"s
      println(actualProgram)
    }
  }
}

class LinearMutatorTest2 extends FunSuite {
  test("gernerate Not/And-Programs") {
    val mutator = new LinearMutator
    mutator.init(new ProblemSpec("", 0, List(Not, And), null))
    var actualProgram : Exp = Box()
    for (_ <- 0 to 100) {
      if (Metadata.size(actualProgram) > 6)
        actualProgram = mutator.stepOver(actualProgram).get
      else
        actualProgram = mutator.stepInto(actualProgram).get
      // should be only "not"s
      println(actualProgram)
    }
  }

//  test("mutate Node") {
//    LinearMutator.init(new ProblemSpec("", 10, List(And, Or, Xor), null))
//    val expected = List(Zero -> One,
//                        One -> MainVar,
//                        MainVar -> And,
//                        And -> Or,
//                        Or -> Xor)
//    expected.foreach((s) => {
//      LinearMutator.mutateNode(s._1) match {
//        case Some(n) => assert(n == s._2)
//        case None => fail
//      }
//    })
//    
//    LinearMutator.mutateNode(Xor) match {
//      case Some(_) => fail
//      case None => 
//    }
//  }
//  
//  test("mutate empty-Exp") {
//    LinearMutator.init(new ProblemSpec("", 10, null, null))
//    assert(LinearMutator.mutate(DoubleLinkedList()).get.equals(DoubleLinkedList(Zero)))
//  }
//  
//  test("mutate at position") {
//    LinearMutator.init(new ProblemSpec("", 10, List(Not), null))
//    val expected = List((DoubleLinkedList(Zero), 0) -> DoubleLinkedList(One),
//                        (DoubleLinkedList(One), 0) -> DoubleLinkedList(Not, Zero),
//                        (DoubleLinkedList(Not, Zero), 1) -> DoubleLinkedList(Not, One),
//                        (DoubleLinkedList(Not, One), 1) -> DoubleLinkedList(Not, Not, Zero))
//    val exceptionExpected = List((DoubleLinkedList(Not, Zero), 0),
//                                 (DoubleLinkedList(Not, One), 0),
//                                 (DoubleLinkedList(Not, Not, Zero), 0),
//                                 (DoubleLinkedList(Not, Not, One), 0),
//                                 (DoubleLinkedList(Not, Not, Zero), 1),
//                                 (DoubleLinkedList(Not, Not, One), 1))
//    expected.foreach(s => assert(LinearMutator.mutateAtPosition(s._1._1, s._1._2).equals(s._2)))
//  }
//  
//  test("generate possible Programs") {
//    LinearMutator.init(new ProblemSpec("", 10, List(Not), null))
//    var actualProgram = DoubleLinkedList[Node]()
//    for (_ <- 0 until 100) {
//      actualProgram = LinearMutator.mutate(actualProgram).get
//      println(actualProgram)
//    }
//  }
}