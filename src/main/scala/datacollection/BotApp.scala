package datacollection

import java.text.SimpleDateFormat
import java.util.Date

object BotApp {
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
  def log(text: String) {
    println("[%s] %s".format(dateFormat.format(new Date()), text))
  }
  def sleep(seconds: Int) {
    log("Waiting %d seconds..".format(seconds))
    val waitUntil = System.currentTimeMillis() + seconds * 1000;
    while (System.currentTimeMillis() < waitUntil) {
      Thread.sleep(waitUntil - System.currentTimeMillis());
    }
  }
}