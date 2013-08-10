package solver.evaluation

import java.io.File
import datacollection.TrainingProblemStore
import solver.solvers.BruteForceSizeFilteredSolver
import datacollection.BotApp
import javax.swing.WindowConstants

object Test extends App {
    val store = new TrainingProblemStore(new File("problems/train3"))
//    val evaluator = new CountCorrectInputsEvaluator((store.allProblems().filter(_.size < 5).take(10)))
//    evaluator.evaluate(new BruteForceSizeFilteredSolver)
//
//  val dataSet = new DefaultXYDataset()
//  dataSet.addSeries("bruteForce", Array[Array[Double]](
//    Array[Double](0, 10, 20, 30, 40, 50, 100),
//    Array[Double](0, .2, .4, .5, .7, .8, .8)))
//  dataSet.addSeries("targettedSearch", Array[Array[Double]](
//    Array[Double](0, 40, 60, 100),
//    Array[Double](0, .5, .9, .9)))
//
//  val chart = ChartFactory.createXYStepChart(
//    "Chart",
//    "Time", "Best Solution Correct %",
//    dataSet,
//    PlotOrientation.VERTICAL,
//    true, // legend
//    true, // tooltips
//    false // urls
//    );
//  chart.getPlot().asInstanceOf[XYPlot].getRangeAxis().setRange(0, 1)
//  val base = 0//-1*60*60*1000
//  chart.getPlot().asInstanceOf[XYPlot].getDomainAxis().setRange(base, base + 5*60*1000)
//
//  val frame = new ChartFrame("Chart", chart)
//  frame.pack();
//  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
//  frame.setVisible(true)
//
//  Thread.sleep(2000)
//  dataSet.addSeries("bruteForce", Array[Array[Double]](
//    Array[Double](0, 100, 200, 300, 400, 500, 1000, 1100, 1200),
//    Array[Double](0, .2, .4, .5, .7, .8, .8, .9, .9)))
//  dataSet.addSeries("targettedSearch", Array[Array[Double]](
//    Array[Double](0, 400, 600, 1200),
//    Array[Double](0, .5, .9, .9)))
// 
//  Thread.sleep(1000)
//  dataSet.addSeries("bruteForce", Array[Array[Double]](
//    Array[Double](0, 100, 200, 300, 400, 500, 1100, 1500),
//    Array[Double](0, .2, .4, .5, .7, .8, .9, .9)))
//  dataSet.addSeries("targettedSearch", Array[Array[Double]](
//    Array[Double](0, 400, 600, 1300, 1400, 1500),
//    Array[Double](0, .5, .9, .9, 1, 1)))

}
