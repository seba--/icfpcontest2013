package model

import lang.Semantics

case class TrainingProblem(id: String, size: Int, operators: List[String], solved: Boolean, timeLeft: Int, evaluationResults: Map[String, String], challenge: String) {
  lazy val evaluationResultsAsLong = evaluationResults.map{
    case (in, out) => Semantics.fromString(in) -> Semantics.fromString(out)
  }
}
