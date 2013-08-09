package json

import java.io.File
import model.TrainingProblem
import com.fasterxml.jackson.core.JsonFactory
import scala.collection.JavaConversions

object TrainingProblemStatistics extends App {
  type SumMap = Map[Int, Map[Int, Set[String]]]

  case class Sum(all: SumMap, withFold: SumMap, withoutFold: SumMap)

  def count(map: SumMap, problem: TrainingProblem) = {
    val opCount = problem.operators.size
    val bySize = map(problem.size)
    val bySizeAndOpCount = bySize(opCount)
    map + (problem.size -> (bySize + (opCount -> (bySizeAndOpCount + problem.id))))
  }
  def csvPrint(name: String, map: SumMap) {
    val allOpSizes = map.flatMap(_._2.keySet).toSet
    val allProgramSizes = map.keySet
    val minOps = allOpSizes.min
    val maxOps = allOpSizes.max
    val minSize = allProgramSizes.min
    val maxSize = allProgramSizes.max

    val p = new java.io.PrintWriter(new File(name + ".csv"))

    p.print("stats")
    for (col <- minOps to maxOps) {
      p.print(";" + col + " ops")
    }
    for (row <- minSize to maxSize) {
      p.print("\r\n size " + row)
      for (col <- minOps to maxOps) {
        p.print(";" + map(row)(col).size)
      }
    }
    p.close()
  }
  def sum(problems: Iterable[TrainingProblem]) = {
    val innerWithDefault = Map[Int, Set[String]]().withDefaultValue(Set[String]())
    val outerWithDefault = Map[Int, Map[Int, Set[String]]]().withDefaultValue(innerWithDefault)

    problems.foldLeft(Sum(outerWithDefault, outerWithDefault, outerWithDefault)) { (maps, problem) =>

      val allCounted = maps.copy(all = count(maps.all, problem))
      if (problem.operators.contains("fold") || problem.operators.contains("tfold")) {
        allCounted.copy(withFold = count(maps.withFold, problem))
      } else {
        allCounted.copy(withoutFold = count(maps.withoutFold, problem))
      }
    }
  }
  def ids(sum: SumMap, programSize: Int, numberOfOps: Int) = {
    sums.all(programSize)(numberOfOps)
  }

  val parser = new JsonFactory().createJsonParser(new File("problems", "myproblems.txt"));
  def myProblems = JsonParser.mapper.readValues(parser, classOf[TrainingProblem])
  var result = List[TrainingProblem]()
  while(myProblems.hasNext()) {
    result ::= myProblems.next()
  }
  
  val sums = sum(result)
  csvPrint("all", sums.all)
  csvPrint("withFold", sums.withFold)
  csvPrint("withoutFold", sums.withoutFold)

  //  println(ids(sums.all, 3, 1))
}