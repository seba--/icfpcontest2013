package solver.evaluation

import java.io.File
import datacollection.TrainingProblemStore
import solver.solvers.BruteForceSizeFilteredSolver
import datacollection.BotApp

object Test extends App {
  val evaluator = new CountCorrectInputsEvaluator(new TrainingProblemStore(new File("problems/train3")).allProblems.take(10))
  evaluator.evaluate(new BruteForceSizeFilteredSolver)
}