package json

import scalaj.http.Http
import scalaj.http.HttpOptions

object TrainingProblemDownloader {
  def downloadOneInstance() =
    Http("http://icfpc2013.cloudapp.net/train")
      .param("auth", "0256cpBWw1iiQ9BmQnLRWzAEtN9fLxClBk9CYn3e" + "vpsH1H")
      .option(HttpOptions.readTimeout(10000))
      .asString
}