package frame

import client.api.ServerFacade
import server.api.IcfpcServer
import solver.solvers.BruteForceSizeFilteredSolver

object LiveRunner extends App {
  // solve actual problems
  val server = new ServerFacade(IcfpcServer)

  println("fetching open problems from server")
  val size = 11
  val allMatchingProblems = server.myProblems.filter { problem =>
    (problem.size <= size || (problem.size <= size + 4 && problem.operators.contains(lang.Abstract.Operator.TFold))) &&
      !problem.operators.contains(lang.Abstract.Operator.Bonus) &&
      problem.solved != Some(true) && problem.timeLeft != Some(0)
  }
  println("total matching problems left: " + allMatchingProblems.size)

  println("solving up to first 100...")
  val worker = new InteractiveSolveAndGuess(server, allMatchingProblems.take(100).iterator)
  worker(new BruteForceSizeFilteredSolver)
}