package solver

import datacollection.TrainingProblemStore
import java.io.File
import lang.Concrete
import lang.Semantics

object TestSolve extends App {
  // MODIFY THIS TO WHATEVER YOU NEED!
  val store = new TrainingProblemStore(new File("problems/trainWith0to255eval"))
  val problem = store.read("0jsJYBkkAftXcIlUUf4AsOqz")

  // LEAVE THE REST DOWN HERE ALONE!
  val program = Concrete.parse(problem.challenge)._1
  problem.evaluationResults.foreach {
    case (input, output) =>
      val longInput = Semantics.fromString(input)
      val longOutput = Semantics.fromString(output)
      val result = Semantics.eval(program)(longInput)
      if (result == longOutput) {
        println(s"Ok: ${problem.id} with input $input")
      } else {
        println(s"Failed: ${problem.id} with input $input, expected: $output, but was: " + Semantics.toString(result))
      }
  }
}