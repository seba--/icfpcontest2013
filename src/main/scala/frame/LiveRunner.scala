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

object LiveRunner extends App {
  // solve actual problems
  val server = new ServerFacade(IcfpcServer)

  println("fetching open problems from server")
  val allMatchingProblems = server.myProblems.filter { problem =>
    problem.size <= 8 && !problem.operators.contains(lang.Abstract.Operator.Bonus) && problem.solved != Some(true) && problem.timeLeft != Some(0)
  }
  println("total matching problems left: "+allMatchingProblems.size)

  println("solving up to first 100...")
  val worker = new InteractiveSolveAndGuess(server, allMatchingProblems.take(100).iterator)
  worker(new BruteForceSizeFilteredSolver)
}