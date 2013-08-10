package frame

import server.api.LocalServer
import datacollection.TrainingProblemStore
import client.api.ServerFacade
import client.api.Problem
import solver.solvers.BruteForceSizeFilteredSolver

object LocalServerDryRunner extends App {
  val server = new ServerFacade(new LocalServer(TrainingProblemStore.default))
  val problems = TrainingProblemStore.default.allProblems.filter {
    p => p.size <= 8 && !p.operators.contains("tfold")
  }.take(10).map(Problem(_))
  val worker = new InteractiveSolveAndGuess(server, problems)
  worker(new BruteForceSizeFilteredSolver)
}