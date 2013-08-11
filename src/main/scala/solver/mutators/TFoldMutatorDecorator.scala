package solver.mutators

import solver.Mutator
import solver.ProblemSpec
import lang.Abstract._
import lang.Metadata._

class TFoldMutatorDecorator(m: Mutator) extends Mutator {

  var containsTFold = false

  def init(spec: ProblemSpec): Unit = {
    if (spec.operators.contains(Operator.TFold)) {
      containsTFold = true
      m.init(spec.copy(size = spec.size - 4, operators = spec.operators.filterNot((o) => o == Operator.TFold)))
    } else {
      containsTFold = false
      m.init(spec)
    }
  }

  def notifyNewData(data: Map[Long, Long]): Unit = {
    m.notifyNewData(data)
  }

  def stepInto(e: Exp): Option[Exp] = {
    if (containsTFold) {
      doDecoratorMagic(e, m.stepInto)
    } else {
      m.stepInto(e)
    }
  }

  def stepOver(e: Exp): Option[Exp] = {
    if (containsTFold) {
      doDecoratorMagic(e, m.stepOver)
    } else {
      m.stepOver(e)
    }
  }

  def doDecoratorMagic(e: Exp, mFun: (Exp) => Option[Exp]): Option[Exp] = {
    (e match {
      case Fold(_, _, body) =>
        mFun(body)
      case e =>
        mFun(e)
    }).map { Fold(MainVar, Zero, _) }
  }
}