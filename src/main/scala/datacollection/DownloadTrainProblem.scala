package datacollection

import json.JsonParser
import http.IcfpcHttpCommunication

object DownloadTrainProblem extends BotApp {
  while (true) {
    try {
      log("Attempting download..")
      val json = IcfpcHttpCommunication.get("train");
      val parsed = JsonParser.parseProblem(json);

      TrainingProblemStore.default.write(parsed);
      log("wrote problem:" + parsed.id)

      wait(5)
    } catch {
      case e: Exception =>
        log("Exception: " + e.getMessage())
        wait(1)
    }
  }
}