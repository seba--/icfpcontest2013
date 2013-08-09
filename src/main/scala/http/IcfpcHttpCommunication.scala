package http

import scalaj.http.Http
import scalaj.http.HttpOptions

object IcfpcHttpCommunication {
  private val authToken = "0256cpBWw1iiQ9BmQnLRWzAEtN9fLxClBk9CYn3e"
  private val baseUrl = "http://icfpc2013.cloudapp.net/"
  private val baseParams = "?auth="+ authToken + "vpsH1H"

  def makeUrl(command: String) = baseUrl + command + baseParams
  
  def setParamsAndOptionsAndGo(http: Http.Request) = {
      http.option(HttpOptions.readTimeout(10000)).option(HttpOptions.connTimeout(3000)).asString
  }

  def get(command: String) = {
    setParamsAndOptionsAndGo(Http(makeUrl(command)))
  }

  def post(command: String, body: String) = {
    setParamsAndOptionsAndGo(Http.postData(makeUrl(command), body))
  }
}