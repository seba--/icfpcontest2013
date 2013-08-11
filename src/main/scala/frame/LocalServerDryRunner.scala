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

  // solve training problems
//  val worker = new InteractiveSolveAndGuess(server, new Iterator[Problem] {
//    def hasNext = true
//    def next = {
//      BotApp.sleep(4)
//      server.train(10)
//    }
//  })
  
  // solve actual problems
  val worker = new InteractiveSolveAndGuess(server, server.myProblems.filter{problem =>
    problem.size <= 8 && !problem.operators.contains(lang.Abstract.Operator.Bonus) && problem.solved != Some(true) && problem.timeLeft != Some(0)
  }.iterator)
  
  // some individual problems that take an eternity to solve..
  //  val worker = new InteractiveSolveAndGuess(server, Iterable(Problem(ProblemResponse("SpHEsKdE660usXYyYkBVKGx4", 10, List("and", "if0", "not", "shr1", "shr4"), false, 0, null, "(lambda (main_var) (if0 (and (shr1 (shr4 (not main_var))) 1) 0 main_var))"))).iterator)
  // Problem(QSkf9IEM8xR1ZZVbUee6lC1Z,10,List(And, If0, Shl1, Shr1, Shr16),false,0,null,(if0 (and (shl1 main_var) main_var) (shr1 main_var) (shr16 main_var)))

  worker(new BruteForceSizeFilteredSolver)
}