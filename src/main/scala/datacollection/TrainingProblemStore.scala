package datacollection

import java.io.File
import json.JsonParser
import scala.Array.canBuildFrom
import server.api.ProblemResponse

case class TrainingProblemStore(folder: File) {
  if (!folder.isDirectory() && !folder.mkdirs()) throw new IllegalArgumentException("folder must be a directory!")

  def write(problem: ProblemResponse) {
    val p = new java.io.PrintWriter(new File(folder, problem.id))
    try {
      p.write(JsonParser.serialize(problem))
    } finally {
      p.close()
    }
  }

  def read(id: String) = {
    JsonParser.deserialize(scala.io.Source.fromFile(new File(folder, id)).mkString, classOf[ProblemResponse])
  }

  def contains(id: String) = {
    new File(folder, id).isFile()
  }

  def ids() = folder.list();

  def allProblems() = ids.map { read(_) }
}

object TrainingProblemStore {
  val default = new TrainingProblemStore(new File("problems/train"))
}