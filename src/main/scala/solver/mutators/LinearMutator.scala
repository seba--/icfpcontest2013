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
  
  def stepInto(e: Exp): Option[Exp] = {
    e match {
      case b@Box() => if (b.isEmpty) Some(Zero()) else stepInto(b.e)
      case Zero() => Some(One())
      case One() => Some(MainVar())
      case MainVar() => Some(FoldAcc())
      case FoldAcc() => Some(FoldNext())
      case FoldNext() => Some(MutatorUtils.getMinimalExpressionForOperator(specs.operators(0)))
      case ifExp@IfZero(_, _, _) =>
        val newCond = stepInto(ifExp.cond)
        if (newCond.isDefined) {
          ifExp.cond = newCond.get
          Some(ifExp)
        }
        else {
          val newThen = stepInto(ifExp.yes)
          if (newThen.isDefined) {
            ifExp.cond = Zero()
            ifExp.yes = newThen.get
            Some(ifExp)
          }
          else {
            val newElse = stepInto(ifExp.no)
            if (newElse.isDefined) {
              ifExp.cond = Zero()
              ifExp.yes = Zero()
              ifExp.no = newElse.get
              Some(ifExp)
            }
            else
              None
          }
        }
      case uExp@UApp(_, _) => {
        
      }
    }
    
//    if (e == null || size(e) == 1)
//      Some(Zero())
//    else
//      mutateAtPosition(e, 0)
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
  
  def mutateNode(n: Exp) : Option[Exp] = {
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