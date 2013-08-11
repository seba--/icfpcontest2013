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
      m.init(new ProblemSpec(spec.id, spec.size, spec.operators.filterNot((o) => o == Operator.TFold), spec.data))
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
    if (size(e) == 1) {
      val mutated = mFun(e)
      Some(Fold(MainVar(), Zero(), mutated.get))
    } else {
      e match {
        case Fold(_, _, body) => {
          val mutated = mFun(body)
          mutated match {
            case None => None
            case Some(e) => Some(Fold(MainVar(), Zero(), e))
          }
        }
        case _ => throw new IllegalStateException("previous mutation must have generated a TFold but didn't got one")
      }
    }
  }
}