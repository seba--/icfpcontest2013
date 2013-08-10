package solver.mutators

import lang.Abstract._
import scala.util.Either
import scala.collection.mutable.DoubleLinkedList
import solver.Mutator
import solver.ProblemSpec
import lang.Metadata._

object LinearMutator extends Mutator {
  
  var specs : ProblemSpec = null
  
  def init(spec: ProblemSpec): Unit = { specs = spec }
  
  def notifyNewData(data: Map[Long, Long]): Unit = {}
  
  def mutate(e: Exp): Option[Exp] = {
    if (e == null || size(e) == 1)
      Some(Zero())
    else
      mutateAtPosition(e, e.size - 1)
  }
  
  def mutateAtPosition(e : Exp, pos: Int) : Option[Exp] = {
    if (pos < 0)
      return None
    val mutatedOp : Option[Node] = mutateNode(e(pos))
    mutatedOp match {
      case None => mutateAtPosition(e, pos - 1)
      case Some(n) =>
        val args = numOfArgs(n)
        // generate the new operator and the needed arguments
        val newArgs = DoubleLinkedList[Node]() ++ ((0 to args) map (_ => Zero ))
        // replace the old operator by the new one
        var el = e
        for (_ <- 0 to pos)
          el = e.next
        var numOfOldArgs = numOfArgs(el.elem)
        var lastOldArg = el
        while (numOfOldArgs > 0) {
          lastOldArg = lastOldArg.next
          numOfOldArgs -= 1
          numOfOldArgs += numOfArgs(lastOldArg.elem)
        }
        el.elem = n
        el.next = newArgs
        newArgs.prev = el
        val restList = lastOldArg.next
        
        Some(e)
    }
  }
  
  def mutateNode(n: Node) : Option[Node] = {
    n match {
      case Zero => Some(One)
      case One => Some(MainVar)
      case MainVar => Some(specs.operators(0))
      case others =>
        val index = specs.operators.indexOf(n) + 1
        if (index == specs.operators.size) 
          None
        else
          Some(specs.operators(index))
    }
  }
  
  def numOfArgs(n: Exp) : Int = {
    n match {
      case Zero() => 0
      case One() => 0
      case MainVar() => 0
      case FoldNext() => 0
      case FoldAcc() => 0
      case IfZero(_, _, _) => 3
      case UApp(_, _) => 1
      case BApp(_, _, _) => 2
      case Fold(_, _, _) => 3
    }
  }
}