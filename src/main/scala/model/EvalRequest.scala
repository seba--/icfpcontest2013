package model

import datacollection.TrainingProblemStore
import java.io.FileNotFoundException

case class EvalRequest(id: String, arguments: List[String]) {
  try{
    TrainingProblemStore.default.read(id);
  } catch {
    case fnfe: FileNotFoundException =>
      throw new IllegalArgumentException("Must not request evals for non-training problems yet!");
  }
  if (arguments.size <= 0 || arguments.size > 256) throw new IllegalArgumentException("Illegal number of input values!")
}