package datacollection

import server.api.ProblemResponse

object ProblemFinder extends App {
  var min = new ProblemResponse("init", Int.MaxValue, (1 to 50).map{i => ""+i}.toList, false, 0, null, null)
  TrainingProblemStore.default.allProblems.foreach { problem =>
    if(problem.operators.contains("shl1") && !problem.operators.contains("plus")) {
      if(problem.size < min.size || (problem.size == min.size && problem.operators.size < min.operators.size)) {
    	  min = problem
      }
    }
  }
  println(min)
}