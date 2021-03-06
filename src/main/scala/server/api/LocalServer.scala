package server.api

import datacollection.TrainingProblemStore
import client.api.Status
import lang.Semantics
import client.api.Problem
import lang.Concrete
import client.api.OperatorRestriction

class LocalServer(store: TrainingProblemStore) extends Server {
  private val iter = store.allProblems.iterator
  override def guess(request: GuessRequest): GuessResponse = {
    val program = Concrete.parse(request.program)
    if (program == store.read(request.id).asProblem.challenge.get) {
      println("[Server] Correct guess for " + request.id)
      GuessResponse("win", null, null, false)
    } else {
      println("[Server] Wrong guess for " + request.id + ": " + request.program)
      GuessResponse("error", null, "check not properly implemented", false)
    }
  }
  override def eval(request: EvalRequest): EvalResponse = {
    println("[Server] Eval for " + request.id)
    val problem = store.read(request.id).asProblem
    EvalResponse("ok", request.arguments.toList.map { arg =>
      Semantics.toString(Semantics.eval(problem.challenge.get)(Semantics.fromString(arg)))
    }, null)
  }
  override def status(): Status = {
    throw new UnsupportedOperationException
  }
  override def train(trainingRequest: TrainingRequest): ProblemResponse = {
    iter.next
  }
  override def myProblems() = {
    throw new UnsupportedOperationException
  }
}