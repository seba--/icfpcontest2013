package solver.filter

import solver.Filter
import solver.FilterV
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

  def filter(e: Exp): Int = e match {
      case IfZero(cond, e1, e2) => if (!isConst(cond)) Math.max(filter(e1), filter(e2)) else FilterV.STEP_INTO
      case Fold(over, init, body) => if(!isConst(body)) Math.max(filter(over), Math.max(filter(init), filter(body))) else FilterV.STEP_INTO
      case UApp(op, e1) => filter(e1)
      case BApp(op, e1, e2) => Math.max(filter(e1), filter(e2))
      case b@Box() => if (b.isEmpty) FilterV.OK else filter(b.e)
      case _ => FilterV.OK
  }
  
  def isConst(e: Exp) : Boolean = e match {
      case IfZero(cond, _, _) => isConst(cond) 
      case Fold(over, init, body) => isConst(over) && isConst(init) && isConst(body)
      case UApp(op, e1) => isConst(e1)
      case BApp(op, e1, e2) => isConst(e1) && isConst(e2)
      case b@Box() => if (b.isEmpty) false else isConst(b.e)
      case MainVar => false
      case FoldAcc => false
      case FoldNext => false
      case _ => true
    }
}