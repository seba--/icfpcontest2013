package frame

import client.api.ServerFacade
import datacollection.TrainingProblemStore
import server.api.LocalServer
import server.api.ProblemResponse
import solver.solvers.BruteForceSizeFilteredSolver
import lang.Abstract.Operator._
import client.api.Problem

object LocalServerDryRunner extends App {
  val server = new ServerFacade(new LocalServer(TrainingProblemStore.default))

//  val worker = new InteractiveSolveAndGuess(server, Iterable(ProblemResponse("022FWloSxpUdsAAlkCjU6rFA", 12, List("and","if0","not","or","plus","shr1","shr16","tfold","xor"), None, None, None, None).asProblem).iterator)
  // Problem(QSkf9IEM8xR1ZZVbUee6lC1Z,10,List(And, If0, Shl1, Shr1, Shr16),false,0,null,(if0 (and (shl1 main_var) main_var) (shr1 main_var) (shr16 main_var)))
  
  // added 14:50 Problem(d9p5fa4ef3bkf6sF2XKkZ7mk,12,List(And, If0, Shl1, Shr1, Shr16),None,None,None,Some((shr1 (shl1 (if0 (and (and (shr16 0) main_var) main_var) main_var main_var)))))

  // added 16:40
//  val worker = new InteractiveSolveAndGuess(server, Iterable(new Problem("xkSYcn5qeK9WCeyqU9hIdJcZ",12,List(And, Fold, Not, Or, Shl1),None,None,None,None)).iterator)

  // added 17:25
  val worker = new InteractiveSolveAndGuess(server, Iterable(new Problem("GT1mjAKuVEmdpFAfv8nuEWN1",20,List(And, If0, Not, Or, Plus, Shl1, Shr1),None,None,None,None)).iterator)

  worker(new BruteForceSizeFilteredSolver)
}
