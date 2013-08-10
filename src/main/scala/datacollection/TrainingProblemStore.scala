package datacollection

import java.io.File
import json.JsonParser
import scala.Array.canBuildFrom
import server.api.ProblemResponse

case class TrainingProblemStore(folder: File) {
  private var cache = Map[String, ProblemResponse]()

  if (!folder.isDirectory() && !folder.mkdirs()) throw new IllegalArgumentException("folder must be a directory!")

  def write(problem: ProblemResponse) {
    cache += problem.id -> problem
    val p = new java.io.PrintWriter(new File(folder, problem.id))
    try {
      p.write(JsonParser.serialize(problem))
    } finally {
      p.close()
    }
  }

  def read(id: String) = {
    try {
      cache.get(id).getOrElse {
        val result = JsonParser.deserialize(scala.io.Source.fromFile(new File(folder, id)).mkString, classOf[ProblemResponse])
        cache += (id -> result)
        result
      }
    } catch {
      case e: Exception =>
        println(id)
        throw e
    }
  }

  def contains(id: String) = {
    new File(folder, id).isFile()
  }

  def ids() = folder.list().filterNot(_.startsWith("."));

  def allProblems() = ids.map { read(_) }
}

object TrainingProblemStore {
  val default = new TrainingProblemStore(new File("problems/train"))
}