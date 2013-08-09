package json

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import model.TrainingProblem

object JsonParser {
  private val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  def parse(json: String) = mapper.readValue(json, classOf[TrainingProblem])
  def serialize(problem : TrainingProblem) = mapper.writeValueAsString(problem)
}