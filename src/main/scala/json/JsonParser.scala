package json

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import model.TrainingProblem
import model.EvalResponse

object JsonParser {
  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  def parseProblem(json: String) = mapper.readValue(json, classOf[TrainingProblem])
  def parseEvalResponse(json: String) = mapper.readValue(json, classOf[EvalResponse])
  def serialize(someObject : Any) = mapper.writeValueAsString(someObject)
}