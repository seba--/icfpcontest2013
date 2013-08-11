package http

import scalaj.http.Http
import scalaj.http.HttpOptions
import scalaj.http.HttpException
import datacollection.BotApp

object IcfpcHttpCommunication {
  private val authToken = "0256cpBWw1iiQ9BmQnLRWzAEtN9fLxClBk9CYn3e"
  private val baseUrl = "http://icfpc2013.cloudapp.net/"
  private val baseParams = "?auth=" + authToken + "vpsH1H"

  def makeUrl(command: String) = baseUrl + command + baseParams

  def setParamsAndOptionsAndGo(http: Http.Request) = {
    var result : String = null
    while (try {
      result = http.option(HttpOptions.readTimeout(10000)).option(HttpOptions.connTimeout(3000)).asString
      false
    } catch {
      case e: HttpException if (e.getMessage() != null && e.getMessage() == "429: Too many requests") =>
        BotApp.log("Timeout on request, retrying...")
        BotApp.sleep(1)
        true
    }) {}
    result
  }

  def get(command: String) = {
    setParamsAndOptionsAndGo(Http(makeUrl(command)))
  }

  def post(command: String, body: String) = {
    setParamsAndOptionsAndGo(Http.postData(makeUrl(command), body))
  }
}