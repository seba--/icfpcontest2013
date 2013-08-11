package client.api

import lang.Semantics.Value
import lang.Abstract.Exp
import lang.Abstract.Operator
import server.api.Server
import lang.Semantics
import server.api.GuessRequest
import server.api.EvalRequest
import json.JsonParser
import com.fasterxml.jackson.core.JsonFactory
import java.io.File
import datacollection.TrainingProblemStore
import lang.Concrete
import server.api.ProblemResponse

case class Problem(id: String, size: Int, operators: List[Operator], solved: Boolean, timeLeft: Int, evaluationResults: Map[Long, Long], challenge: Exp)
object Problem {
  def convert(problem: ProblemResponse): Problem = {
    val evaluationResultsAsLong = if (problem.evaluationResults == null) null else problem.evaluationResults.map {
      case (in, out) => Semantics.fromString(in) -> Semantics.fromString(out)
    }
    val operatorsToOperators = problem.operators.map {
      Concrete.tryParseOperator(_).get
    }
    val challengeToExp = {
      if (problem.challenge == null) null else Concrete.parse(problem.challenge)
    }
    Problem(problem.id, problem.size, operatorsToOperators, problem.solved, problem.timeLeft, evaluationResultsAsLong, challengeToExp)
  }
}
sealed trait GuessResponse
case object Win extends GuessResponse
case class Mismatch(input: Value, correct: Value, wrong: Value) extends GuessResponse
case class Error(message: String) extends GuessResponse
object GuessResponse {
  def apply(response: server.api.GuessResponse) = {
    response.status match {
      case "win" => Win
      case "mismatch" => Mismatch(Semantics.fromString(response.values(0)), Semantics.fromString(response.values(1)), Semantics.fromString(response.values(2)))
      case "error" => Error(response.message)
    }
  }
}
case class Window(resetsIn: Int, amount: Int, limit: Int)
case class Status(easyChairId: String, contestScore: Int, lightningScore: Int, trainingScore: Int, mismatches: Int, numRequests: Int, requestWindow: Window, cpuWindow: Window, cpuTotalTime: Int)

// TODO implement all methods
class ServerFacade(theServer: Server) {
  val trainingStore = new TrainingProblemStore(new File("problems/train3"))
  def myProblems(): Seq[Problem] = {
    val parser = new JsonFactory().createJsonParser(new File("problems", "myproblems.txt"));
    val myProblems = JsonParser.mapper.readValues(parser, classOf[ProblemResponse])
    var result = List[Problem]()
    while (myProblems.hasNext()) {
      result ::= myProblems.next().asProblem
    }
//    result
    throw new SecurityException
  }
  val trainingProblems: Seq[Problem] = trainingStore.allProblems.map { _.asProblem }
  def guess(id: String, program: Exp): GuessResponse = {
    GuessResponse(theServer.guess(new GuessRequest(id, s"(lambda (main_var) ${program.toString})")))
  }
  def eval(id: String, arguments: Seq[Long]): Seq[(Long, Long)] = {
    val response = theServer.eval(new EvalRequest(id, Semantics.toStringList(arguments)))
    arguments.zip(Semantics.fromStringList(response.get))
  }
  def status(): Status = {
    theServer.status
  }
  def train(size: Int = 0): Problem = {
    theServer.train(size).asProblem
  }
}