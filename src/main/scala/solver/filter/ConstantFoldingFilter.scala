package solver.filter

import scala.collection.mutable.DoubleLinkedList
import solver.Filter
import scala.collection.immutable.Map
import solver.ProblemSpec
import lang.Abstract._

/**
 * Ensures that the expression contains no constant folding sub-expressions,
 * i.e., no if0 with constant conditions and no fold with constant body.
 */
class ConstantFoldingFilter extends Filter {
  var spec: ProblemSpec = null

  def init(spec: ProblemSpec) {
    this.spec = spec
  }

  def notifyNewData(delta: Map[Long, Long]) {
    // ignore
  }

  def filter(e: Exp): Boolean = e match {
      case IfZero(cond, e1, e2) => !isConst(cond) && filter(e1) && filter(e2) 
      case Fold(over, init, body) => !isConst(body) && filter(over) && filter(init) && filter(body)
      case UApp(op, e1) => filter(e1)
      case BApp(op, e1, e2) => filter(e1) && filter(e2)
      case b@Box() => if (b.isEmpty) true else filter(b.e)
      case _ => true
  }
  
  def isConst(e: Exp) : Boolean = e match {
      case IfZero(cond, _, _) => isConst(cond) 
      case Fold(over, init, body) => isConst(over) && isConst(init) && isConst(body)
      case UApp(op, e1) => isConst(e1)
      case BApp(op, e1, e2) => isConst(e1) && isConst(e2)
      case b@Box() => if (b.isEmpty) false else isConst(b.e)
      case MainVar() => false
      case FoldAcc() => false
      case FoldNext() => false
      case _ => true
    }
}