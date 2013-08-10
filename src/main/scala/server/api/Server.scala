package server.api

import client.api.Status
import lang.Semantics
import datacollection.TrainingProblemStore
import java.io.FileNotFoundException
import lang.Abstract
import lang.Concrete

case class ProblemResponse(id: String, size: Int, operators: List[String], solved: Boolean, timeLeft: Int, evaluationResults: Map[String, String], challenge: String)

case class EvalRequest(id: String, arguments: Seq[String]) {
  try{
    TrainingProblemStore.default.read(id);
  } catch {
    case fnfe: FileNotFoundException =>
      throw new IllegalArgumentException("Must not request evals for non-training problems yet!");
  }
  if (arguments.size <= 0 || arguments.size > 256) throw new IllegalArgumentException("Illegal number of input values!")
}

case class EvalResponse (status: String, outputs: List[String], message: String){
  def get = status match {
    case "ok" => outputs
    case "error" => throw new IllegalArgumentException(message)
    case _ => throw new Exception("unknown status: "+status);
  }
}

case class GuessRequest(id: String, program: String)
case class GuessResponse(status: String, values: List[String], message: String, lightning: Boolean)

trait Server {
  def guess(request: GuessRequest): GuessResponse
  def eval(request: EvalRequest): EvalResponse
  def status(): Status
}