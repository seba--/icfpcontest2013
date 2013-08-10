package solver.mutators

import lang.Abstract._
import scala.util.Either
import scala.collection.mutable.DoubleLinkedList
import solver.Mutator
import solver.ProblemSpec

object LinearMutator extends Mutator {
  
  var specs : ProblemSpec = null
  
  def init(spec: ProblemSpec): Unit = { specs = spec }
  
  def notifyNewData(data: Map[Long, Long]): Unit = {}
  
  def stepInto(e: Exp): Option[Exp] = null
  def stepOver(e: Exp): Option[Exp] = null
  
//  {
//    if (e.size == 0)
//      Some(DoubleLinkedList(Zero))
//    else
//      // TODO check: if mutateAtPosition can fail, it should return Option[Exp]
//      Some(mutateAtPosition(e, specs.operators, e.size - 1))
//  }
//  
//  def mutateAtPosition(e : Exp, op: List[Node], pos: Int) : Exp = {
//    val nextOp : Either[Node, String] = mutateNode(e(pos))
//    nextOp match {
//      // if there is no further possible node, alternate the previous one
//      case Right(_) => mutateAtPosition(e, op, pos - 1)
//      case Left(n) =>
//        val args = numOfArgs(n)
//        // generate the new operator and the needed arguments
//        val newOp = DoubleLinkedList(n)
//        for (_ <- 0 to args)
//          newOp ++ DoubleLinkedList(Zero)
//        // replace the old operator by the new one
//        var el = e
//        for (_ <- 1 to pos)
//          el = e.next
//        el.next = newOp
//        newOp.prev = el
//        // if the new Exp is acceptable regarding the length then return it
//        if (e.size + 1 > specs.size) // TODO: Check the "Add 1" for the program's lambda-expression
//          mutateAtPosition(e, op, pos)
//        else
//          e
//    }
//  }
//  
//  def mutateNode(n: Node) : Either[Node, String] = {
//    n match {
//      case Zero => Left(One)
//      case One => Left(MainVar)
//      case MainVar => Left(specs.operators(0))
//      case others => val index = specs.operators.indexOf(n) + 1
//        if (index == specs.operators.size) 
//          Right("No more operators")
//        else
//          Left(specs.operators(index))
//    }
//  }
//  
//  def numOfArgs(n: Node) : Int = {
//    n match {
//      case Zero => 0
//      case One => 0
//      case MainVar => 0
//      case IfZero => 3
//      case Not => 1
//      case Shl1 => 1
//      case Shr1 => 1
//      case Shr4 => 1
//      case Shr16 => 1
//      case And => 2
//      case Or => 2
//      case Xor => 2
//      case Plus => 2
//      // TODO: Add a handling for fold
//    }
//  }
}