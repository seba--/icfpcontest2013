package solver.mutators

import lang.Abstract._
import scala.util.Either
import scala.collection.mutable.DoubleLinkedList
import solver.Mutator
import solver.ProblemSpec
import lang.Metadata._

object LinearMutator extends Mutator {

  var ops: List[Operator] = List()

  def init(spec: ProblemSpec): Unit = { ops = spec.operators.filterNot((o) => o == Operator.Bonus) }

  def notifyNewData(data: Map[Long, Long]): Unit = {}

  var lastModified: Option[Exp] = None
  var newLastModified: Option[Exp] = None

  def stepOver(e: Exp): Option[Exp] = {
    if (lastModified.isEmpty)
      throw new IllegalStateException("lastModified is empty")
    newLastModified = None
    val result = stepOver_(e)
    lastModified = newLastModified
    result match {
      case Left(e) => Some(e)
      case Right(true) => {
        val newExp = MutatorUtils.getNextMinimalExpression(e, ops)
        if (newExp.isEmpty)
          None
        else {
          lastModified = newExp
          newExp
        }
      }
      case Right(false) => throw new IllegalStateException("expression does not contain the last modified expression")
    }
  }

  def stepOver_(e: Exp): Either[Exp, Boolean] = {
    if (e == lastModified.get)
      Right(true)
    else {
      val modification = e match {
        case b @ Box() => if (b.isEmpty) Right(false) else stepOver_(b.e)
        case Zero => Right(false)
        case One => Right(false)
        case MainVar => Right(false)
        case FoldNext => Right(false)
        case FoldAcc => Right(false)
        case ifExp @ IfZero(_, _, _) => {
          stepOver_(ifExp.cond) match {
            case Left(e) =>
              ifExp.cond = e
              Left(ifExp)
            case Right(true) => {
              val newExp = MutatorUtils.getNextMinimalExpression(ifExp.cond, ops)
              if (newExp.isEmpty)
                stepInto(ifExp.yes) match {
                  case None => stepInto(ifExp.no) match {
                    case None => Right(true)
                    case Some(e) => {
                      ifExp.no = e
                      ifExp.yes = Zero
                      ifExp.cond = Zero
                      Left(ifExp)
                    }
                  }
                  case Some(e) => {
                    ifExp.yes = e
                    ifExp.cond = Zero
                    Left(ifExp)
                  }
                }
              else {
                ifExp.cond = newExp.get
                newLastModified = newExp
                Left(ifExp)
              }
            }
            case Right(false) =>
              stepOver_(ifExp.yes) match {
                case Left(e) =>
                  ifExp.yes = e
                  Left(ifExp)
                case Right(true) => {
                  val newExp = MutatorUtils.getNextMinimalExpression(ifExp.yes, ops)
                  if (newExp.isEmpty)
                    stepInto(ifExp.no) match {
                      case None => Right(true)
                      case Some(e) => {
                        ifExp.no = e
                        ifExp.yes = Zero
                        ifExp.cond = Zero
                        Left(ifExp)
                      }
                    }
                  else {
                    ifExp.yes = newExp.get
                    newLastModified = newExp
                    Left(ifExp)
                  }
                }
                case Right(false) =>
                  stepOver_(ifExp.no) match {
                    case Left(e) =>
                      ifExp.no = e
                      Left(ifExp)
                    case Right(true) => {
                      val newExp = MutatorUtils.getNextMinimalExpression(ifExp.no, ops)
                      if (newExp.isEmpty)
                        Right(true)
                      else {
                        ifExp.no = newExp.get
                        newLastModified = newExp
                        Left(ifExp)
                      }
                    }
                    case Right(false) => Right(false)
                  }
              }

          }
        }
        case fold @ Fold(_, _, _) => {
          stepOver_(fold.over) match {
            case Left(e) =>
              fold.over = e
              Left(fold)
            case Right(true) => {
              val newExp = MutatorUtils.getNextMinimalExpression(fold.over, ops)
              if (newExp.isEmpty)
                stepInto(fold.init) match {
                  case None => stepInto(fold.body) match {
                    case None => Right(true)
                    case Some(e) => {
                      fold.body = e
                      fold.init = Zero
                      fold.over = Zero
                      Left(fold)
                    }
                  }
                  case Some(e) => {
                    fold.init = e
                    fold.over = Zero
                    Left(fold)
                  }
                }
              else {
                fold.over = newExp.get
                newLastModified = newExp
                Left(fold)
              }
            }
            case Right(false) =>
              stepOver_(fold.init) match {
                case Left(e) =>
                  fold.init = e
                  Left(fold)
                case Right(true) => {
                  val newExp = MutatorUtils.getNextMinimalExpression(fold.init, ops)
                  if (newExp.isEmpty)
                    stepInto(fold.body) match {
                      case None => Right(true)
                      case Some(e) => {
                        fold.body = e
                        fold.init = Zero
                        fold.over = Zero
                        Left(fold)
                      }
                    }
                  else {
                    fold.init = newExp.get
                    newLastModified = newExp
                    Left(fold)
                  }
                }
                case Right(false) =>
                  stepOver_(fold.body) match {
                    case Left(e) =>
                      fold.body = e
                      Left(fold)
                    case Right(true) => {
                      val newExp = MutatorUtils.getNextMinimalExpression(fold.body, ops)
                      if (newExp.isEmpty)
                        Right(true)
                      else {
                        fold.body = newExp.get
                        newLastModified = newExp
                        Left(fold)
                      }
                    }
                    case Right(false) => Right(false)
                  }
              }

          }
        }
        case uExp @ UApp(_, _) => {
          stepOver_(uExp.e) match {
            case Left(e) =>
              uExp.e = e
              Left(uExp)
            case Right(true) => {
              val newExp = MutatorUtils.getNextMinimalExpression(uExp.e, ops)
              if (newExp.isEmpty)
                Right(true)
              else {
                uExp.e = newExp.get
                newLastModified = newExp
                Left(uExp)
              }
            }
            case Right(false) => Right(false)
          }
        }
        case bExp @ BApp(_, _, _) => {
          stepOver_(bExp.e1) match {
            case Left(e) =>
              bExp.e1 = e
              Left(bExp)
            case Right(true) => {
              val newExp = MutatorUtils.getNextMinimalExpression(bExp.e1, ops)
              if (newExp.isEmpty)
                stepInto(bExp.e2) match {
                  case None => Right(true)
                  case Some(e) => {
                    bExp.e2 = e
                    bExp.e1 = Zero
                    Left(bExp)
                  }
                }
              else {
                bExp.e1 = newExp.get
                newLastModified = newExp
                Left(bExp)
              }
            }
            case Right(false) =>
              stepOver_(bExp.e2) match {
                case Left(e) =>
                  bExp.e2 = e
                  Left(bExp)
                case Right(true) => {
                  val newExp = MutatorUtils.getNextMinimalExpression(bExp.e2, ops)
                  if (newExp.isEmpty)
                    Right(true)
                  else {
                    bExp.e2 = newExp.get
                    newLastModified = newExp
                    Left(bExp)
                  }
                }
                case Right(false) => Right(false)
              }
          }
        }
      }
      if (newLastModified.isEmpty)
        modification match {
          case Left(e) => newLastModified = Some(e)
          case Right(_) => {}
        }
      modification
    }
  }

  def stepInto(e: Exp): Option[Exp] = {
    newLastModified = None
    val result = stepInto_(e)
    lastModified = newLastModified
    result
  }

  def stepInto_(e: Exp): Option[Exp] = {
    val modification = e match {
      case b @ Box() => if (b.isEmpty) Some(Zero) else stepInto(b.e)
      case Zero => Some(One)
      case One => Some(MainVar)
      case MainVar => Some(FoldAcc)
      case FoldAcc => Some(FoldNext)
      case FoldNext => Some(MutatorUtils.getMinimalExpressionForOperator(ops(0)))
      case ifExp @ IfZero(_, _, _) => {
        val newCond = stepInto(ifExp.cond)
        if (newCond.isDefined) {
          ifExp.cond = newCond.get
          Some(ifExp)
        } else {
          val newThen = stepInto(ifExp.yes)
          if (newThen.isDefined) {
            ifExp.cond = Zero
            ifExp.yes = newThen.get
            Some(ifExp)
          } else {
            val newElse = stepInto(ifExp.no)
            if (newElse.isDefined) {
              ifExp.cond = Zero
              ifExp.yes = Zero
              ifExp.no = newElse.get
              Some(ifExp)
            } else
              MutatorUtils.getNextMinimalExpression(ifExp, ops)
          }
        }
      }
      case uExp @ UApp(_, _) => {
        val newSub = stepInto(uExp.e)
        if (newSub.isDefined) {
          uExp.e = newSub.get
          Some(uExp)
        } else
          MutatorUtils.getNextMinimalExpression(uExp, ops)
      }
      case bExp @ BApp(_, _, _) => {
        val newLeft = stepInto(bExp.e1)
        if (newLeft.isDefined) {
          bExp.e1 = newLeft.get
          Some(bExp)
        } else {
          val newRight = stepInto(bExp.e2)
          if (newRight.isDefined) {
            bExp.e1 = Zero
            bExp.e2 = newRight.get
            Some(bExp)
          } else
            MutatorUtils.getNextMinimalExpression(bExp, ops)
        }
      }
      case fold @ Fold(_, _, _) => {
        val newOver = stepInto(fold.over)
        if (newOver.isDefined) {
          fold.over = newOver.get
          Some(fold)
        } else {
          val newInit = stepInto(fold.init)
          if (newInit.isDefined) {
            fold.over = Zero
            fold.init = newInit.get
            Some(fold)
          } else {
            val newBody = stepInto(fold.body)
            if (newBody.isDefined) {
              fold.over = Zero
              fold.init = Zero
              fold.body = newBody.get
              Some(fold)
            } else
              MutatorUtils.getNextMinimalExpression(fold, ops)
          }
        }
      }
    }
    if (newLastModified.isEmpty && modification.isDefined)
      newLastModified = modification
    modification
  }
}