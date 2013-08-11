package solver.solvers

import solver.Strategy
import solver.Canceled
import lang.Abstract.Exp
import solver.Mutator
import solver.mutators.TFoldMutatorDecorator
import solver.mutators.LinearMutator
import solver.ProblemSpec
import solver.Filter
import solver.Fitness
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.collection.mutable.Queue
import lang.Concrete
import solver.strategies.BruteForceInitialDataStrategy
import lang.Abstract
import lang.Abstract.Operator
import lang.Abstract.Operator._
import solver.mutators.MutatorUtils
import solver.FilterV._
import datacollection.BotApp
import scala.collection.mutable

class TopExpressionRestriction(op: Operator) extends Filter {
  def init(spec: ProblemSpec) {}
  def notifyNewData(data: Map[Long, Long]) {}

  def filter(e: Exp) = {
    e match {
      case Abstract.Fold(Abstract.MainVar, Abstract.Zero, e) =>
        test(e);
      case e => test(e);
    }
  }

  def test(e: Exp) = {
    if (Abstract.getOperator(e) == Some(op))
      OK
    else
      STEP_OVER

  }
}
class ParallelBruteForceStrategy extends Strategy {
  var interrupted = false
  var strategies: Map[Operator, Strategy] = _
  val processes: mutable.Set[Operator] = mutable.Set()

  def init(spec: ProblemSpec, mutator: Mutator, filter: Filter, fitness: Fitness) = {
    interrupted = false

    processes.clear();
    processes ++= spec.operators.filter(op => op != Bonus && op != TFold)
    strategies = processes.map { operator =>
      val strategy = new BruteForceInitialDataStrategy(MutatorUtils.getMinimalExpressionForOperator(operator))
      strategy.init(spec, new TFoldMutatorDecorator(new LinearMutator), new TopExpressionRestriction(operator) :: filter, fitness)
      operator -> strategy
    }.toMap

    strategies.foreach {
      case (operator, strategy) =>
        val process = future {
          //          BotApp.log("Starting sub-search for " + operator)
          while (strategy.nextSolution() match {
            case Some(e) =>
              //              BotApp.log("Solution from strategy " + operator + ": " + e.toString)
              val ec = Concrete.parse(s"(lambda (main_var) $e)")
              queue.synchronized {
                queue += ec
                queue.notifyAll()
              }
              true
            case None =>
              processes.synchronized {
                processes -= operator
              }
              queue.synchronized {
                queue.notifyAll()
              }
              false
          }) {}
          //          BotApp.log("Sub-search for " + operator + " terminated.")
        }.onFailure {
          case Canceled =>
          // ignore
          case e: Exception =>
            BotApp.log("Sub-search for " + operator + " crashed!")
            e.printStackTrace()
        }
    }
  }

  def isDone() = {
    processes.synchronized(processes.isEmpty)
  }

  // notify about new evaluation data, will be already registered in problem spec
  def notifyNewData(delta: Map[Long, Long]): Unit = {
    strategies.values.foreach { _.notifyNewData(delta) }
  }

  val queue = Queue[Exp]()

  def nextSolution(): Option[Exp] = {
    queue.synchronized {
      while (!interrupted && queue.isEmpty && !isDone) {
        //        BotApp.log("Now waiting for solution..")
        queue.wait()
        //        BotApp.log("Got notify.")
      }
      if (interrupted || queue.isEmpty) {
        None
      } else {
        Some(queue.dequeue)
      }
    }
  }

  def interrupt() {
    interrupted = true
    queue.synchronized { queue.notifyAll() }
    strategies.values.foreach(_.interrupt())
  }
}