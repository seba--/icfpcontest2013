package solver.evaluation

import java.io.File
import datacollection.TrainingProblemStore
import solver.solvers.BruteForceSizeFilteredSolver
import datacollection.BotApp

object Test extends App {
  val store = new TrainingProblemStore(new File("problems/train3"))
  val evaluator = new CountCorrectInputsEvaluator(((store.ids()).take(10).map(store.read(_))))
  evaluator.evaluate(new BruteForceSizeFilteredSolver)
}