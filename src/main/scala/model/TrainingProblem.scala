package model

case class TrainingProblem(id: String, size: Int, operators: List[String], solved: Boolean, timeLeft: Int, evaluationResults: Map[String, String], challenge: String)
