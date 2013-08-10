package frame

import solver.Solver
import model.TrainingProblem
import scala.collection.mutable.Queue
import solver.ProblemSpec
import model.EvalRequest
import model.EvalRequest
import lang.Semantics
import datacollection.EvalDownload
import scala.concurrent._
import ExecutionContext.Implicits.global
import datacollection.BotApp
import lang.Abstract.Exp
import lang.Concrete
import model.Guess
import http.IcfpcHttpCommunication
import json.JsonParser
import model.GuessResponse

object InteractiveSolveAndGuess {
  val problems = Queue[TrainingProblem]()
  val numbers = Seq[Semantics.Value]()

  def apply(solver: Solver) {
    while (!problems.isEmpty) {
      val problem = problems.dequeue()
      val iter = numbers.iterator
      def downloadNextEvalResults() = EvalDownload.requestEvalResultsInLong(problem.id, iter.take(256).toSeq)
      val spec = ProblemSpec(problem)

      val timeout = System.currentTimeMillis() + 5 * 60 * 1000
      def timeLeft() = timeout - System.currentTimeMillis()
      val initialResults = downloadNextEvalResults()
      spec.data ++= initialResults
      solver.init(spec)
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
            if (solutions.isEmpty) None else solutions.dequeue()
          } match {
            case Some(program) =>
              val guess = Guess(problem.id, program.toString)
              val response = IcfpcHttpCommunication.post("guess", JsonParser.serialize(guess))
              val result = JsonParser.deserialize(response, classOf[GuessResponse])
              result.status match {
                case "win" =>
                //TODO
                case "error" =>
                //TODO
                case "mismatch" =>
                  val in = Semantics.fromString(result.values(0))
                  val out = Semantics.fromString(result.values(1))
                  spec.data += (in -> out)
                  solver.notifyNewData(Map(in -> out))
              }
            case None =>
              val newResults = downloadNextEvalResults()
              spec.data ++= newResults
              solver.notifyNewData(newResults.toMap)
          }
        }
      }
    }
  }
}