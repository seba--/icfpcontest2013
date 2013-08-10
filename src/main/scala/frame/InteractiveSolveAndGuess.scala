package frame

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

object InteractiveSolveAndGuess {
  val server : ServerFacade = null
  val problems = Queue[Problem]()
  val numbers = Seq[Value]()

  def apply(solver: Solver) {
    while (!problems.isEmpty) {
      val problem = problems.dequeue()
      val iter = numbers.iterator
      def downloadNextEvalResults() = server.eval(problem.id, iter.take(256).toSeq)

      val timeout = System.currentTimeMillis() + 5 * 60 * 1000
      def timeLeft() = timeout - System.currentTimeMillis()
      
      val initialResults = downloadNextEvalResults()
      solver.init(problem)
      
      val solutions = Queue[Exp]()

      while (timeLeft() > 0) {
        future {
          while (solver.nextSolution() match {
            case Some(e) =>
              solutions.synchronized {
                solutions += e
              }
              true
            case None =>
              false
          }) {}
        }

        while (timeLeft() > 0) {
          BotApp.sleep(4)

          solutions.synchronized {
            if (solutions.isEmpty) None else Some(solutions.dequeue())
          } match {
            case Some(program) =>
              server.guess(problem.id, program) match {
                case Win =>
                //TODO
                case Error(message) =>
                //TODO
                case Mismatch(in, out, _) =>
                  solver.notifyNewData(Map(in -> out))
              }
            case None =>
              val newResults = downloadNextEvalResults()
              solver.notifyNewData(newResults.toMap)
          }
        }
        // TODO store problem
      }
    }
  }
}