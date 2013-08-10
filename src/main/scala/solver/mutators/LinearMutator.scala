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
  
  def stepOver(e: Exp): Option[Exp] = {
    None
  }
  
  def stepInto(e: Exp): Option[Exp] = {
    e match {
      case b@Box() => if (b.isEmpty) Some(Zero()) else stepInto(b.e)
      case Zero() => Some(One())
      case One() => Some(MainVar())
      case MainVar() => Some(FoldAcc())
      case FoldAcc() => Some(FoldNext())
      case FoldNext() => Some(MutatorUtils.getMinimalExpressionForOperator(specs.operators(0)))
      case ifExp@IfZero(_, _, _) => {
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
              MutatorUtils.getNextMinimalExpression(ifExp, specs.operators)
          }
        }
      }
      case uExp@UApp(_, _) => {
        val newSub = stepInto(uExp.e)
        if (newSub.isDefined) {
          uExp.e = newSub.get
          Some(uExp)
        }
        else 
          MutatorUtils.getNextMinimalExpression(uExp, specs.operators)
      }
      case bExp@BApp(_, _, _) => {
        val newLeft = stepInto(bExp.e1)
        if (newLeft.isDefined) {
          bExp.e1 = newLeft.get
          Some(bExp)
        }
        else {
          val newRight = stepInto(bExp.e2)
          if (newRight.isDefined) {
            bExp.e1 = Zero()
            bExp.e2 = newRight.get
            Some(bExp)
          }
          else
            MutatorUtils.getNextMinimalExpression(bExp, specs.operators)
        }
      }
      case fold@Fold(_, _, _) => {
        val newOver = stepInto(fold.over)
        if (newOver.isDefined) {
          fold.over = newOver.get
          Some(fold)
        }
        else {
          val newInit = stepInto(fold.init)
          if (newInit.isDefined) {
            fold.over = Zero()
            fold.init = newInit.get
            Some(fold)
          }
          else {
            val newBody = stepInto(fold.body)
            if (newBody.isDefined) {
              fold.over = Zero()
              fold.init = Zero()
              fold.body = newBody.get
              Some(fold)
            }
            else
              MutatorUtils.getNextMinimalExpression(fold, specs.operators)
          }
        }
      }
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