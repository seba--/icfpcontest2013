package server.api

import datacollection.TrainingProblemStore
import client.api.Status
import lang.Semantics
import client.api.Problem

class LocalServer(store: TrainingProblemStore) extends Server {
  override def guess(request: GuessRequest): GuessResponse = {
    if (request.program == store.read(request.id)) {
      println("[Server] Correct guess for " + request.id)
      GuessResponse("win", null, null, false)
    } else {
      println("[Server] Wrong guess for " + request.id + ": " + request.program)
      GuessResponse("error", null, "check not properly implemented", false)
    }
  }
  override def eval(request: EvalRequest): EvalResponse = {
    println("[Server] Eval for " + request.id)
    val problem = Problem(store.read(request.id))
    EvalResponse("ok", request.arguments.toList.map { arg =>
      Semantics.toString(Semantics.eval(problem.challenge)(Semantics.fromString(arg)))
    }, null)
  }
  override def status(): Status = {
    throw new UnsupportedOperationException
  }
}