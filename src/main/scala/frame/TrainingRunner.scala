package frame

import scala.collection.Iterator
import client.api._
import datacollection.BotApp
import solver.solvers.BruteForceSizeFilteredSolver
import server.api.IcfpcServer
import solver.solvers.PartialSolver
import solver.solvers.DispatchSolver

object TrainingRunner extends App {
  val server = new ServerFacade(IcfpcServer)

  val theSize = 9;

  val worker = new InteractiveSolveAndGuess(server, new Iterator[Problem] {
    def hasNext = true
    def next = {
      BotApp.sleep(4)
      if (math.random < .75) {
        server.train(WithTFold, theSize)
      } else {
        server.train(WithTFold, theSize + 4)
      }
    }
  })

  worker(new BruteForceSizeFilteredSolver)
}