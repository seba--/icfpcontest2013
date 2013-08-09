package solver

import datacollection.TrainingProblemStore
import java.io.File
import lang.Concrete
import lang.Semantics
import scala.collection.SortedMap

object TestSolve extends App {
  // MODIFY THIS TO WHATEVER YOU NEED!
  val store = new TrainingProblemStore(new File("problems/trainWith0to255eval"))
  
  var failed: Set[String] = Set()
  for (id <- store.ids) {
  val problem = store.read(id)

  // LEAVE THE REST DOWN HERE ALONE!
  val program = Concrete.parse(problem.challenge)._1
  (SortedMap[String,String]() ++ problem.evaluationResults).foreach {
    case (input, output) =>
      val longInput = Semantics.fromString(input)
      val longOutput = Semantics.fromString(output)
      val result = Semantics.eval(program)(longInput)
      if (result == longOutput) {
        println(s"Ok: ${problem.id} with input $input")
      } else {
        failed = failed + problem.id
        println(s"Failed: ${problem.id} with input $input, expected: $output, but was: " + Semantics.toString(result))
      }
  }
  }
  failed.foreach(p=>println(p + "," + store.read(p).size))
}