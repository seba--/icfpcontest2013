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
import model.EvalResponse

object EvalDownload extends BotApp {
  def requestEvalResults(problem: TrainingProblem, inputs: List[String]): List[(String, String)] = requestEvalResults(problem.id, inputs)

  def requestEvalResults(id: String, arguments: List[String]): List[(String, String)] = {
    val request = EvalRequest(id, arguments);
    val result = IcfpcHttpCommunication.post("eval", JsonParser.serialize(request))
    arguments.zip(JsonParser.deserialize(result, classOf[EvalResponse]).get)
  }

  def doDownloads(inputStore: TrainingProblemStore, targetStore: TrainingProblemStore, requests: Iterable[Long]) {
    val translatedRequests = requests.toList.map { Semantics.toString(_) }
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

        val newResults = requestEvalResults(problem, translatedRequests)
        val updatedResults = if (problem.evaluationResults == null) {
          newResults.toMap
        } else {
          problem.evaluationResults ++ newResults
        }
        val updatedProblem = problem.copy(evaluationResults = updatedResults)
        targetStore.write(updatedProblem);
        log("done problem: " + problem.id + ", " + toDownload.size + " items left.")
        wait(4)
      } catch {
        case e: Exception =>
          log("Exception: " + e.getMessage())
          // requeue for later processing
          toDownload += problem
          wait(1)
      }
    }
  }

  val singleBitSet = for (i <- (0 to 63)) yield { 1L << i }
  val twoBitsSetEquidistant = for (i <- (0 to 31)) yield { ((1L << 32) + 1L) << i }
  val twoBitsSetVShape = for (i <- (0 to 31)) yield { (1L << (63 - i)) + (1L << i) }
  val bitsSet = singleBitSet ++ twoBitsSetEquidistant ++ twoBitsSetVShape
  val bitsUnset = bitsSet.map { ~_ }
  val requests = bitsSet ++ bitsUnset

  val tempStore = TrainingProblemStore(new File("problems/trainWith0to255eval"))
  val finalStore = TrainingProblemStore(new File("problems/train3"))
//  doDownloads(TrainingProblemStore.default, tempStore, 0L to 255L)
  doDownloads(tempStore, finalStore, requests)
}