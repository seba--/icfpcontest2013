package frame

import datacollection.BotApp._
import solver.Solver
import scala.collection.mutable.Queue
import solver.ProblemSpec
import lang.Semantics
import datacollection.EvalDownload
import scala.concurrent._
import solver.MyExecutionContext._
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
import scala.util.Failure
import scala.util.Success
import javax.xml.ws.http.HTTPException

object InteractiveSolveAndGuess {
  private val singleBits = for (i <- 0 to 63) yield (1L << i)
  private val oneBitPerByte = for (i <- 0 to 7) yield (0x0101010101010101L << i)
  private val twoBitPerByte = for (i <- 0 to 7) yield (0x1111111111111111L << i)
  private val bitPatterns = singleBits ++ oneBitPerByte ++ twoBitPerByte
  private val primes = Primes.primesUnder(1000)
  private val patternNumbers = Seq(~0L, 0L, ~0x5555555555555555L, 0x5555555555555555L) ++ bitPatterns ++ bitPatterns.map { ~_ } ++ primes ++ primes.map { _ + 1 }
  def fillWithRandom(initial: Seq[Long], size: Int) = {
    val random = new Random
    var numbers = initial
    while (numbers.size < size) {
      val next = random.nextLong()
      if (!numbers.contains(next)) {
        numbers = numbers :+ next
      }
    }
    numbers
  }
  val numbers = fillWithRandom(patternNumbers, 256 * 75)
}

class InteractiveSolveAndGuess(val server: ServerFacade, problems: Iterator[Problem]) {
  def apply(solver: Solver) {
    while (problems.hasNext) {
      try {
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
        def filterSolutions(in: Value, out: Value) = {
          solutions.synchronized {
            (solutions.dequeueAll { program =>
              val actual = Semantics.eval(program)(in)
              val correct = actual == out
              //                      println("[Solver] "+(if (correct) "passed" else "dropped (" + Semantics.toString(actual) + ")") + ": " + program.toString())
              !correct
            }.size, solutions.size)
          }
        }

        val solverPollingThread = future {
          log("[Solver] starting search")
          while (solver.nextSolution() match {
            case Some(e) =>
              log("[Solver] new guess: " + e.toString())
              val ec = Concrete.parse(s"(lambda (main_var) $e)")
              solutions.synchronized {
                solutions += ec
              }
              true
            case None =>
              false
          }) {}
          log("[Solver] no more guesses, solver terminated.")
        }
        def killSolver() = {
          if (!solverPollingThread.isCompleted) {
            log("[Interact] Killing solver...")
            solver.interrupt()
            Await.ready(solverPollingThread, Duration(500, MILLISECONDS))
          }
        }

        try {
          def sholdContinue() = problem.solved != Some(true) && timeLeft() > 0 && (!solverPollingThread.isCompleted || !solutions.isEmpty)

          var i = 0
          while (sholdContinue()) {
            BotApp.sleep(1)
            i = i + 1

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
                    log("[Interact] Discarding guess due to error: " + message)
                  case Mismatch(in, out, _) =>
                    log("[Interact] Mismatch for input " + Semantics.toString(in) + ", should result in " + Semantics.toString(out))
                    problem = problem.copy(evaluationResults = problem.evaluationResults.map { _ + (in -> out) })
                    solver.notifyNewData(Map(in -> out))
                    val (wrong, left) = filterSolutions(in, out)
                    if (wrong != 0 || left != 0) log("[Interact] Dequeued %d incorrect solutions, %d left".format(wrong, left))
                }
              case None =>
                if (i > 12) {
                  log("[Interact] No guesses in queue, sending eval. (%.2f seconds left)".format(timeLeft / 1000.0))
                  i = 0
                  val newResults = downloadNextEvalResults()
                  problem = problem.copy(evaluationResults = problem.evaluationResults.map { _ ++ newResults })
                  solver.notifyNewData(newResults.toMap)
                  val (wrong, left) = newResults.foldLeft((0, 0)) {
                    case ((wrong, left), (in, out)) =>
                      val (newWrong, newLeft) = filterSolutions(in, out)
                      (wrong + newWrong, newLeft)
                  }
                  if (wrong != 0 || left != 0) log("[Interact] Dequeued %d incorrect solutions, %d left".format(wrong, left))
                } else {
                  log("[Interact] No guesses in queue, but still on Countdown... (%.2f seconds left)".format(timeLeft / 1000.0))
                }
            }
          }
        } catch {
          case e: HTTPException if (e.getMessage() == "412: already solved" || e.getMessage() == "410: time limit exceeded") =>
            log("[Interact] problem ended, skipping to next problem.");
        }
        killSolver()
        if (problem.solved != Some(true)) {
          log("[Interact] problem aborted. Reasons:")
          if (timeLeft() < 0) {
            log("[Interact] Problem timed out.")
          } else {
            solverPollingThread.value.get match {
              case Failure(e) =>
                log("[Interact] Solver crashed!")
                e.printStackTrace()
              case Success(_) =>
                log("[Interact] Solver terminated with no further solutions.")
            }
          }
        }
        // TODO store problem
      } catch {
        case e: HTTPException if (e.getMessage() == "412: already solved" || e.getMessage() == "410: time limit exceeded") =>
          log("[Interact] problem ended, skipping to next problem.");
      }
    }
    log("[Interact] No more problems, terminating.")
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