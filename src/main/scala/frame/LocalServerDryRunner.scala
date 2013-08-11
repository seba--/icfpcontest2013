package frame

import server.api.LocalServer
import datacollection.TrainingProblemStore
import client.api.ServerFacade
import client.api.Problem
import solver.solvers.BruteForceSizeFilteredSolver
import server.api.IcfpcServer
import scala.collection.Iterator
import datacollection.BotApp
import server.api.ProblemResponse

object LocalServerDryRunner extends App {
  //  val server = new ServerFacade(new LocalServer(TrainingProblemStore.default))
  val server = new ServerFacade(IcfpcServer)

  val worker = new InteractiveSolveAndGuess(server, new Iterator[Problem] {
    def hasNext = true
    def next = {
      BotApp.sleep(4)
      server.train(10)
    }
  })
  //  val worker = new InteractiveSolveAndGuess(server, Iterable(Problem(ProblemResponse("SpHEsKdE660usXYyYkBVKGx4", 10, List("and", "if0", "not", "shr1", "shr4"), false, 0, null, "(lambda (main_var) (if0 (and (shr1 (shr4 (not main_var))) 1) 0 main_var))"))).iterator)

  worker(new BruteForceSizeFilteredSolver)
}