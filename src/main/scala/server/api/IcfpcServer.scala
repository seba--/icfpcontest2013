package server.api

import json.JsonParser
import http.IcfpcHttpCommunication
import client.api.Status
import com.fasterxml.jackson.core.JsonFactory
import client.api.OperatorRestriction

object IcfpcServer extends Server {
  override def guess(request: GuessRequest): GuessResponse = {
    val result = IcfpcHttpCommunication.post("guess", JsonParser.serialize(request))
    JsonParser.deserialize(result, classOf[GuessResponse])
  }
  override def eval(request: EvalRequest): EvalResponse = {
    val result = IcfpcHttpCommunication.post("eval", JsonParser.serialize(request))
    JsonParser.deserialize(result, classOf[EvalResponse])
  }
  override def status(): Status = {
    val result = IcfpcHttpCommunication.get("status")
    JsonParser.deserialize(result, classOf[Status])
  }
  override def train(trainingRequest: TrainingRequest): ProblemResponse = {
    val result = IcfpcHttpCommunication.post("train", JsonParser.serialize(trainingRequest))
    JsonParser.deserialize(result, classOf[ProblemResponse])
  }
  override def myProblems() = {
    val response = IcfpcHttpCommunication.get("myproblems")
    val parser = new JsonFactory().createJsonParser(response.substring(1, response.size - 1).replaceAllLiterally("},", "}"));
    def myProblems = JsonParser.mapper.readValues(parser, classOf[ProblemResponse])
    var result = List[ProblemResponse]()
    while (myProblems.hasNext()) {
      result ::= myProblems.next()
    }
    result
  }
}