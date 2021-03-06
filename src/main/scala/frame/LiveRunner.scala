package frame

import client.api.ServerFacade
import server.api.IcfpcServer
import solver.solvers.BruteForceSizeFilteredSolver
import scala.util.Sorting
import client.api.Problem
import lang.Abstract.Operator._

object LiveRunner extends App {
  // solve actual problems
  val server = new ServerFacade(IcfpcServer)

  val worker = new InteractiveSolveAndGuess(server, new Iterator[Problem] {
    private var theNext = findNext()
    def findNext() = {
      val allMatchingProblems = server.myProblems.toArray
      Sorting.quickSort(allMatchingProblems)(Ordering.by[Problem, Int] { problem =>
        if (problem.operators.contains(TFold)) {
          (problem.size - 4) * (problem.operators.size - 1)
        } else {
          problem.size * problem.operators.size
        }
      })
      allMatchingProblems.find(next => next.solved != Some(true) && next.timeLeft == None)
    }
    def next = {
      val result = theNext
      theNext = findNext()
      theNext.get
    }
    def hasNext = theNext.isDefined
  })
  worker(new BruteForceSizeFilteredSolver)
}