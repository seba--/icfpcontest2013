package model

import scala.util._

case class EvalResponse (status: String, outputs: List[String], message: String){
  def get = status match {
    case "ok" => outputs
    case "error" => throw new IllegalArgumentException(message)
    case _ => throw new Exception("unknown status: "+status);
  }
}