package datacollection

import server.api.ProblemResponse

object ProblemFinder extends App {
  var min = new ProblemResponse("init", Int.MaxValue, (1 to 50).map{i => ""+i}.toList, None, None, None, None)
  TrainingProblemStore.default.allProblems.foreach { problem =>
    if(problem.operators.contains("shl1") && !problem.operators.contains("plus")) {
      if(problem.size < min.size || (problem.size == min.size && problem.operators.size < min.operators.size)) {
    	  min = problem
      }
    }
  }
  println(min)
}