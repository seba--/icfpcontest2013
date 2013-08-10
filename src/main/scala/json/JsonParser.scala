package json

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule

object JsonParser {
  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)

  def deserialize[T](json: String, clazz: Class[T]): T = mapper.readValue(json, clazz)
  def serialize(someObject : Any) = mapper.writeValueAsString(someObject)
}