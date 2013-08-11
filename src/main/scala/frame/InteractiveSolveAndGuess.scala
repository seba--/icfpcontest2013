package frame

import datacollection.BotApp._
import solver.Solver
import scala.collection.mutable.Queue
import solver.ProblemSpec
import lang.Semantics
import datacollection.EvalDownload
import scala.concurrent._
import ExecutionContext.Implicits.global
import datacollection.BotApp
import lang.Abstract.Exp
import lang.Concrete
import http.IcfpcHttpCommunication
import json.JsonParser
import client.api.Problem
import lang.Semantics.Value
import client.api._
import scala.collection.immutable.SortedSet
import scala.util.Random
import scala.concurrent.duration._

object InteractiveSolveAndGuess {
  private val singleBits = for (i <- 0 to 63) yield (1L << i)
  private val oneBitPerByte = for (i <- 0 to 7) yield (0x0101010101010101L << i)
  private val twoBitPerByte = for (i <- 0 to 7) yield (0x1111111111111111L << i)
  private val bitPatterns = singleBits ++ oneBitPerByte ++ twoBitPerByte
  private val primes = Primes.primesUnder(1000)
  private val patternNumbers = Set(~0L, 0L, ~0x5555555555555555L, 0x5555555555555555L) ++ bitPatterns ++ bitPatterns.map { ~_ } ++ primes ++ primes.map { _ + 1 }
  def fillWithRandom(initial: Set[Long], size: Int) = {
    val random = new Random
    var numbers = initial
    while (numbers.size < size) {
      numbers += random.nextLong()
    }
    numbers
  }
  val numbers = fillWithRandom(patternNumbers, 256 * 75)
}

class InteractiveSolveAndGuess(val server: ServerFacade, problems: Iterator[Problem]) {
  def apply(solver: Solver) {
    while (problems.hasNext) {
      {
        var problem = problems.next()
        log("Now solving problem: " + problem)
        val iter = InteractiveSolveAndGuess.numbers.iterator
        def downloadNextEvalResults() = server.eval(problem.id, iter.take(256).toSeq)

        val timeout = System.currentTimeMillis() + 5 * 60 * 1000
        def timeLeft() = timeout - System.currentTimeMillis()

        val initialResults = downloadNextEvalResults()
        solver.init(problem.copy(evaluationResults = Some(initialResults.toMap)))
        problem = problem.copy(evaluationResults = Some(
          problem.evaluationResults match {
            case None => initialResults.toMap
            case Some(results) => results ++ initialResults.toMap
          }))

        val solutions = Queue[Exp]()
        def filterSolutions(in: Value, out: Value) {
          solutions.synchronized {
            log("[Interact] Dequeued %d incorrect solutions, %d left".format(solutions.dequeueAll { program =>
              val actual = Semantics.eval(program)(in)
              val correct = actual == out
              //                      println("[Solver] "+(if (correct) "passed" else "dropped (" + Semantics.toString(actual) + ")") + ": " + program.toString())
              !correct
            }.size, solutions.size))
          }
        }

        val solverPollingThread = future {
          while (solver.nextSolution() match {
            case Some(e) =>
              log("[Solver] new guess: " + e.toString())
              val ec = Concrete.parse(s"(lambda (main_var) $e)")
              solutions.synchronized {
                solutions += ec
              }
              true
            case None =>
              log("[Solver] no more guesses, solver terminated.")
              false
          }) {}
        }
        def killSolver() = {
          if (!solverPollingThread.isCompleted) {
            log("[Interact] Killing solver...")
            solver.interrupt()
            Await.result(solverPollingThread, Duration(100, MILLISECONDS))
          }
        }

        def sholdContinue() = problem.solved != Some(true) && timeLeft() > 0 && (!solverPollingThread.isCompleted || !solutions.isEmpty)

        while (sholdContinue()) {
          BotApp.sleep(4)

          solutions.synchronized {
            if (solutions.isEmpty) None else Some(solutions.dequeue())
          } match {
            case Some(program) =>
              log("[Interact] Sending guess: " + program.toString())
              server.guess(problem.id, program) match {
                case Win =>
                  log("[Interact] Correct guess!")
                  problem = problem.copy(solved = Some(true))
                  killSolver()
                case Error(message) =>
                  log("[Interact] Error: " + message)
                //TODO do something
                case Mismatch(in, out, _) =>
                  log("[Interact] Mismatch for input " + Semantics.toString(in) + ", should result in " + Semantics.toString(out))
                  problem = problem.copy(evaluationResults = problem.evaluationResults.map { _ + (in -> out) })
                  solver.notifyNewData(Map(in -> out))
                  filterSolutions(in, out)
              }
            case None =>
              log("[Interact] No guesses in queue, sending eval.")
              val newResults = downloadNextEvalResults()
              problem = problem.copy(evaluationResults = problem.evaluationResults.map { _ ++ newResults })
              solver.notifyNewData(newResults.toMap)
              newResults.foreach {
                case (in, out) =>
                  filterSolutions(in, out)
              }
          }
        }
        killSolver()
        // TODO store problem
      }
    }
  }
}

object Primes {
  def primesUnder(n: Long): List[Long] = {
    require(n >= 2)

    def rec(i: Long, primes: List[Long]): List[Long] = {
      if (i >= n) primes
      else if (prime(i, primes)) rec(i + 1, i :: primes)
      else rec(i + 1, primes)
    }

    rec(2, List()).reverse
  }

  def prime(num: Long, factors: List[Long]): Boolean = factors.forall(num % _ != 0)
}