package json

import java.io.File
import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.Date

object DownloadTrainProblem extends App {
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
  def log(text: String) {
    println("[%s] %s".format(dateFormat.format(new Date()), text))
  }
  def wait(seconds: Int) {
    log("Waiting %d seconds..".format(seconds))
    val waitUntil = System.currentTimeMillis() + seconds * 1000;
    while (System.currentTimeMillis() < waitUntil) {
      Thread.sleep(waitUntil - System.currentTimeMillis());
    }
  }

  while (true) {
    try {
      log("Attempting download..")
      val json = TrainingProblemDownloader.downloadOneInstance();
      val parsed = JsonParser.parse(json);

      TrainingProblemStore.write(parsed);
      log("wrote problem:" + parsed.id)

      wait(5)
    } catch {
      case e: Exception =>
        log("Exception: " + e.getMessage())
        wait(1)
    }
  }

}