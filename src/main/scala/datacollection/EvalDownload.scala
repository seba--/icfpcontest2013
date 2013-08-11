package datacollection

import http.IcfpcHttpCommunication
import json.JsonParser
import lang.Semantics
import scala.collection.SortedMap
import scala.collection.mutable.Queue
import java.io.File
import com.sun.xml.internal.bind.v2.TODO
import BotApp._
import server.api.ProblemResponse
import server.api.IcfpcServer
import server.api.EvalRequest

object EvalDownload extends App {
  def requestEvalResults(problem: ProblemResponse, inputs: Seq[String]): Seq[(String, String)] = requestEvalResults(problem.id, inputs)

  def requestEvalResultsInLong(id: String, arguments: Seq[Long]): Seq[(Long, Long)] = {
    requestEvalResults(id, Semantics.toStringList(arguments)).map {
      case (in, out) => Semantics.fromString(in) -> Semantics.fromString(out)
    }
  }

  def requestEvalResults(id: String, arguments: Seq[String]) = {
    arguments.zip(IcfpcServer.eval(new EvalRequest(id, arguments)).get)
  }

  def doDownloads(inputStore: TrainingProblemStore, targetStore: TrainingProblemStore, requests: Iterable[Long]) {
    val translatedRequests = requests.toList.map { Semantics.toString(_) }
    val toDownload = Queue[ProblemResponse]()
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
        val updatedResults = problem.evaluationResults match {
          case None => Some(newResults.toMap)
          case Some(results) => Some(results ++ newResults)
        }
        val updatedProblem = problem.copy(evaluationResults = updatedResults)
        targetStore.write(updatedProblem);
        log("done problem: " + problem.id + ", " + toDownload.size + " items left.")
        sleep(4)
      } catch {
        case e: Exception =>
          log("Exception: " + e.getMessage())
          // requeue for later processing
          toDownload += problem
          sleep(1)
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
  doDownloads(TrainingProblemStore.default, tempStore, 0L to 255L)
  doDownloads(tempStore, finalStore, requests)
}