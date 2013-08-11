package frame

import client.api.ServerFacade
import datacollection.TrainingProblemStore
import server.api.LocalServer
import server.api.ProblemResponse
import solver.solvers.BruteForceSizeFilteredSolver

object LocalServerDryRunner extends App {
  val server = new ServerFacade(new LocalServer(TrainingProblemStore.default))

  val worker = new InteractiveSolveAndGuess(server, Iterable(ProblemResponse("SpHEsKdE660usXYyYkBVKGx4", 10, List("and", "if0", "not", "shr1", "shr4"), None, None, None, Some("(lambda (main_var) (if0 (and (shr1 (shr4 (not main_var))) 1) 0 main_var))")).asProblem).iterator)
  // Problem(QSkf9IEM8xR1ZZVbUee6lC1Z,10,List(And, If0, Shl1, Shr1, Shr16),false,0,null,(if0 (and (shl1 main_var) main_var) (shr1 main_var) (shr16 main_var)))

  worker(new BruteForceSizeFilteredSolver)
}