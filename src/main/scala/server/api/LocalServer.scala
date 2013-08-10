package server.api

import datacollection.TrainingProblemStore
import client.api.Status
import lang.Semantics

class LocalServer(store: TrainingProblemStore) extends Server {
  override def guess(request: GuessRequest): GuessResponse = {
    throw new UnsupportedOperationException
  }
  override def eval(request: EvalRequest): EvalResponse = {
    val problem = store.read(request.id).toClientProblem
    EvalResponse("ok", request.arguments.toList.map { arg =>
      Semantics.toString(Semantics.eval(problem.challenge)(Semantics.fromString(arg)))
    }, null)
  }
  override def status(): Status = {
    throw new UnsupportedOperationException
  }
}