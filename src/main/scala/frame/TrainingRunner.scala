package frame

import scala.collection.Iterator
import client.api._
import datacollection.BotApp
import solver.solvers.BruteForceSizeFilteredSolver
import server.api.IcfpcServer

object TrainingRunner extends App {
  val server = new ServerFacade(IcfpcServer)

  val worker = new InteractiveSolveAndGuess(server, new Iterator[Problem] {
    def hasNext = true
    def next = {
      BotApp.sleep(4)
      server.train(WithTFold, 16)
    }
  })
  
  worker(new BruteForceSizeFilteredSolver)
}