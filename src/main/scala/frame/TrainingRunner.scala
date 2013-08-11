package frame

import scala.collection.Iterator

import client.api.Problem
import client.api.ServerFacade
import datacollection.BotApp
import server.api.IcfpcServer
import solver.solvers.BruteForceSizeFilteredSolver

object TrainingRunner extends App {
  val server = new ServerFacade(IcfpcServer)

  val worker = new InteractiveSolveAndGuess(server, new Iterator[Problem] {
    def hasNext = true
    def next = {
      BotApp.sleep(4)
      server.train(11)
    }
  })
  
  worker(new BruteForceSizeFilteredSolver)
}