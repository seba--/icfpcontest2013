package model

case class GuessResponse(status: String, values: List[String], message: String, lightning: Boolean)
