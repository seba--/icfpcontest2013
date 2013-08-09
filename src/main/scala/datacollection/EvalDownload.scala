package datacollection

import model.TrainingProblem
import model.EvalRequest
import http.IcfpcHttpCommunication
import json.JsonParser
import lang.Semantics
import scala.collection.SortedMap
import scala.collection.mutable.Queue
import java.io.File
import com.sun.xml.internal.bind.v2.TODO

object EvalDownload extends BotApp {
  def requestEvalResults(problem: TrainingProblem, inputs: List[String]): List[(String, String)] = requestEvalResults(problem.id, inputs)

  def requestEvalResults(id: String, arguments: List[String]): List[(String, String)] = {
    val request = EvalRequest(id, arguments);
    val result = IcfpcHttpCommunication.post("eval", JsonParser.serialize(request))
    arguments.zip(JsonParser.parseEvalResponse(result).get)
  }

  val inputStore = TrainingProblemStore.default
  val targetStore = TrainingProblemStore(new File("problems/trainWith0to255eval"))
  val requests = (0L to 255L).toList.map { Semantics.toString(_) }
  val toDownload = Queue[TrainingProblem]()
  inputStore.ids().foreach {
    (id =>
      if (!targetStore.contains(id)) toDownload += inputStore.read(id))
  }
  log("Starting: " + toDownload.size + " problems in queue")

  while (!toDownload.isEmpty) {
    val problem = toDownload.dequeue()
    try {
      log("Attempting to eval for " + problem.id)

      val newResults = EvalDownload.requestEvalResults(problem, requests)
      val updatedResults = if (problem.evaluationResults == null) {
        newResults.toMap
      } else {
        problem.evaluationResults ++ newResults
      }
      val updatedProblem = problem.copy(evaluationResults = updatedResults)
      targetStore.write(updatedProblem);
      log("done problem: " + problem.id + ", " + toDownload.size + " items left.")
      wait(5)
    } catch {
      case e: Exception =>
        log("Exception: " + e.getMessage())
        // requeue for later processing
        toDownload += problem
        wait(1)
    }
  }
}