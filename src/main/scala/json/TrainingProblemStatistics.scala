package json

import java.io.File
import model.TrainingProblem

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
    for (col <- minOps until maxOps) {
      p.print(";" + col + " ops")
    }
    for (row <- minSize until maxSize) {
      p.print("\r\n size " + row)
      for (col <- minOps until maxOps) {
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

  val sums = sum(TrainingProblemStore.allProblems)
  csvPrint("all", sums.all)
  csvPrint("withFold", sums.withFold)
  csvPrint("withoutFold", sums.withoutFold)

  //  println(ids(sums.all, 3, 1))
}