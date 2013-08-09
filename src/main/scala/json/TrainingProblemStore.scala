package json

import model.TrainingProblem
import java.io.File

object TrainingProblemStore {
  private val folder = new File("problems/train");
  def write(problem: TrainingProblem) {
    val p = new java.io.PrintWriter(new File("problems/train/" + problem.id))
    try {
      p.write(JsonParser.serialize(problem))
    } finally {
      p.close()
    }
  }

  def read(id: String) = JsonParser.parse(scala.io.Source.fromFile(new File(folder, id)).mkString)

  def ids() = folder.list();
}