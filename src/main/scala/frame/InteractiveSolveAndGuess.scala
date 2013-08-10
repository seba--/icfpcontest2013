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

object InteractiveSolveAndGuess {
  private val primes = Primes.primesUnder(1000)
  private val singleBits = for (i <- 0 to 63) yield (1L << i)
  private val oneBitPerByte = for (i <- 0 to 7) yield (0x0101010101010101L << i)
  private val twoBitPerByte = for (i <- 0 to 7) yield (0x1111111111111111L << i)
  private val bitPatterns = singleBits ++ oneBitPerByte ++ twoBitPerByte
  private val patternNumbers = SortedSet(~0L, 0L, ~0x5555555555555555L, 0x5555555555555555L) ++ primes ++ primes.map { _ + 1 } ++ bitPatterns ++ bitPatterns.map { ~_ }
  def fillWithRandom(initial: SortedSet[Long], size: Int) = {
    val random = new Random
    var numbers = initial
    while (numbers.size < size) {
      numbers += random.nextLong()
    }
    numbers
  }
  val numbers = fillWithRandom(patternNumbers, 256 * 75)
}

class InteractiveSolveAndGuess(val server: ServerFacade, allProblems: Iterable[Problem]) {
  val problems = Queue[Problem]() ++ allProblems

  def apply(solver: Solver) {
    while (!problems.isEmpty) {
      var problem = problems.dequeue()
      val iter = InteractiveSolveAndGuess.numbers.iterator
      def downloadNextEvalResults() = server.eval(problem.id, iter.take(256).toSeq)

      val timeout = System.currentTimeMillis() + 5 * 60 * 1000
      def timeLeft() = timeout - System.currentTimeMillis()
      def sholdContinue() = !problem.solved && timeLeft() > 0

      val initialResults = downloadNextEvalResults()
      solver.init(problem.copy(evaluationResults = initialResults.toMap))
      problem = problem.copy(evaluationResults =
        if (problem.evaluationResults == null)
          initialResults.toMap
        else
          problem.evaluationResults ++ initialResults.toMap)

      val solutions = Queue[Exp]()

      while (sholdContinue()) {
        val solverPollingThread = future {
          while (solver.nextSolution() match {
            case Some(e) =>
              log("[Solver] new guess: "+e.toString())
              solutions.synchronized {
                solutions += e
              }
              true
            case None =>
              log("[Solver] no more guesses.")
              false
          }) {}
        }

        while (!solverPollingThread.isCompleted && sholdContinue()) {
          BotApp.sleep(4)

          solutions.synchronized {
            if (solutions.isEmpty) None else Some(solutions.dequeue())
          } match {
            case Some(program) =>
              server.guess(problem.id, program) match {
                case Win =>
                  problem = problem.copy(solved = true)
                case Error(message) =>
                //TODO
                case Mismatch(in, out, _) =>
                  problem = problem.copy(evaluationResults = problem.evaluationResults + (in -> out))
                  solver.notifyNewData(Map(in -> out))
              }
            case None =>
              val newResults = downloadNextEvalResults()
              problem = problem.copy(evaluationResults = problem.evaluationResults ++ newResults)
              solver.notifyNewData(newResults.toMap)
          }
        }
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