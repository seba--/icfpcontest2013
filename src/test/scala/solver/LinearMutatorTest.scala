//package solver
//
//import org.scalatest.FunSuite
//import lang.FlatAbstract._
//import lang.FlatAbstract.Node._
//import scala.collection.mutable.DoubleLinkedList
//import solver.mutators.LinearMutator
//
//class LinearMutatorTest extends FunSuite {
//  test("nextNode") {
//    val seq = List[Node](And, Or, Xor)
//    LinearMutator.init(new ProblemSpec("", 10, seq, null))
//    val expected = List(Zero -> One,
//                        One -> MainVar,
//                        MainVar -> And,
//                        And -> Or,
//                        Or -> Xor)
//    expected.foreach((s) => {testNextNode(s._1, s._2, seq)})
//    
//    val response : Either[Node, String] = LinearMutator.mutateNode(Xor)
//    response match {
//      case Left(_) => fail
//      case Right(_) => 
//    }
//  }
//  
//  def testNextNode(input: Node, output: Node, seq: Seq[Node]) = {
//    val result : Either[Node, String] = LinearMutator.mutateNode(input)
//    result match {
//      case Right(_) => fail
//      case Left(n) => assert(output == n)
//    }
//  }
//  
//  test("empty -> next") {
//    LinearMutator.init(new ProblemSpec("", 10, null, null))
//    assert(LinearMutator.mutate(DoubleLinkedList()) == DoubleLinkedList(Zero))
//  }
//}