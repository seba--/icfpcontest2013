package http

import scalaj.http.Http
import scalaj.http.HttpOptions

object HttpCommunication {
  private val authToken = "0256cpBWw1iiQ9BmQnLRWzAEtN9fLxClBk9CYn3e"
  private val baseUrl = "http://icfpc2013.cloudapp.net/"

  def setParamsAndOptionsAndGo(http: Http.Request, params: (String, String)*) = {
    http.param("auth", authToken + "vpsH1H")
      .params(params: _*)
      .option(HttpOptions.readTimeout(10000))
      .asString
  }

  def get(command: String, params: (String, String)*) = {
    setParamsAndOptionsAndGo(Http(baseUrl + command), params: _*)
  }

  def post(command: String, body: String, params: (String, String)*) = {
    setParamsAndOptionsAndGo(Http.postData(baseUrl + command, body), params: _*)
  }
}