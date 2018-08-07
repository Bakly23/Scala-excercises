package observatory

import java.io.File

import scala.util.Random

object Main extends App {
  val temps = Extraction.locateTemperatures(2015, "/stations.csv", "/2015.csv")
  val avgTempsByLocation = Extraction.locationYearlyAverageRecords(temps)
  println(avgTempsByLocation)
  val predictedT = Visualization.predictTemperature(avgTempsByLocation, Location(28.967,119.868))
  println(predictedT)
  val tempsToColors = List(
    (-60.0, Color(0, 0, 0)),
    (-50.0, Color(33, 0, 107)),
    (-27.0, Color(255, 0, 255)),
    (-15.0, Color(0, 0, 255)),
    (0.0, Color(0, 255, 255)),
    (12.0, Color(255, 255, 0)),
    (32.0, Color(255, 0, 0)),
    (60.0, Color(255, 255, 255))
  )
  val testScale = List((0.0,Color(255,0,0)), (2.147483647E9,Color(0,0,255)))
  val color = Visualization.interpolateColor(testScale, 1.0737418235E9)
  println(color)
  println(Visualization.interpolateColor(tempsToColors, 61))
  println(Visualization.interpolateColor(tempsToColors, -61))
  val img = Visualization.visualize(Random.shuffle(avgTempsByLocation).take(25000), tempsToColors)
  img.output(new File("target/1982.png"))
}
