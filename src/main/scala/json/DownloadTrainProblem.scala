package json

import java.io.File

object DownloadTrainProblem extends App {
  val parsed = JsonParser.parse(json);
  val json = TrainingProblemDownloader.downloadOneInstance();

  TrainingProblemStore.write(parsed);
  println("wrote:" +parsed.id)
}