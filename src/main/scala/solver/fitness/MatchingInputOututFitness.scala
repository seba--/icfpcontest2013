package solver.fitness

import scala.collection.mutable.DoubleLinkedList
import solver.Fitness
import scala.collection.immutable.Map
import solver.ProblemSpec
import lang.Abstract._
import lang.Semantics._

class MatchingInputOututFitness extends Fitness {
  var spec : ProblemSpec = null
  
  def init(spec: ProblemSpec) { this.spec = spec }

  def notifyNewData(data: Map[Long, Long]) {}

  def fitness(e: Exp) : Double = {
    try {
      return quality(e)
    } catch {
      case _: Exception => return 0.toDouble
    }
  }

  private def quality(e: Exp): Double = {
    val p = eval(e)(_)
    var quality = 0
    spec.data foreach {
      t => if (p(t._1) == t._2) quality += 1;
    }
    return quality / (spec.data.size.toDouble);
  }
}