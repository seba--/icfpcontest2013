package solver.filter

import lang.Abstract._
import lang.Abstract.Operator._
import solver._

class BinaryComparisonFilter extends Filter {
  def init(spec: ProblemSpec): Unit = {}
  def notifyNewData(data: Map[Long, Long]): Unit = {}
  def filter(e: Exp): Int = {
    e match {
      case b @ Box() => if (b.isEmpty) FilterV.OK else filter(b.e)
      case UApp(_, e) => filter(e)
      case IfZero(cond, yes, no) => Math.max(filter(cond), Math.max(filter(yes), filter(no)))
      case lang.Abstract.Fold(over, init, body) => Math.max(filter(over), Math.max(filter(init), filter(body)))
      case BApp(_, e1, e2) => Math.max(Math.max(filter(e1), filter(e2)),
                                 if (compare(e1, e2) < 0) FilterV.STEP_INTO else FilterV.OK)
      case _ => FilterV.OK
    }
  }
  def compare(e1: Exp, e2: Exp): Int = {
    val diff = valueOf(e1) - valueOf(e2)
    if (diff != 0)
      diff
    else {
      e1 match {
        case b @ Box() => 0
        case Zero() => 0
        case One() => 0
        case MainVar() => 0
        case FoldAcc() => 0
        case FoldNext() => 0
        case fold1 @ lang.Abstract.Fold(_, _, _) =>
          e2 match {
            case fold2 @ lang.Abstract.Fold(_, _, _) =>
              val c1 = compare(fold1.over, fold2.over)
              if (c1 == 0) {
                val c2 = compare(fold1.init, fold2.init)
                if (c2 == 0) {
                  compare(fold1.body, fold2.body)
                } else
                  c2
              } else
                c1
          }
        case if1 @ IfZero(_, _, _) =>
          e2 match {
            case if2 @ IfZero(_, _, _) =>
              val c1 = compare(if1.cond, if2.cond)
              if (c1 == 0) {
                val c2 = compare(if1.yes, if2.yes)
                if (c2 == 0) {
                  compare(if1.no, if2.no)
                } else
                  c2
              } else
                c1
          }
        case u1 @ UApp(_, _) =>
          e2 match {
            case u2 @ UApp(_, _) => compare(u1.e, u2.e)
          }
        case b1 @ BApp(_, _, _) =>
          e2 match {
            case b2 @ BApp(_, _, _) =>
              val c1 = compare(b1.e1, b2.e1)
              if (c1 == 0) {
                compare(b1.e2, b2.e2)
              } else
                c1
          }
      }
    }
  }

  def valueOf(e: Exp): Int = {
    e match {
      case b @ Box() => if (b.isEmpty) 0 else valueOf(e)
      case Zero() => 1
      case One() => 2
      case MainVar() => 3
      case FoldAcc() => 4
      case FoldNext() => 5
      case BApp(And, _, _) => 6
      case lang.Abstract.Fold(_, _, _) => 7
      case IfZero(_, _, _) => 8
      case UApp(Not, _) => 9
      case BApp(Or, _, _) => 10
      case BApp(Plus, _, _) => 11
      case UApp(Shl1, _) => 12
      case UApp(Shr1, _) => 13
      case UApp(Shr4, _) => 14
      case UApp(Shr16, _) => 15
      case BApp(Xor, _, _) => 16
    }
  }
}