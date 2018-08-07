package observatory

import java.lang.Math._
import java.util.concurrent.ConcurrentHashMap

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  val EARTH_RADIUS = 6371
  val POWER_PARAMETER = 2.0

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val sortedScale = colors.toSeq.sortBy(_._1)
    val pixels = (90 to -89 by -1).par
      .flatMap(y => (-180 to 179)
        .map(x => interpolateColorSorted(sortedScale, predictTemperature(temperatures, Location(y, x))))
        .map(color => Pixel(color.red, color.green, color.blue, 255))
      )
      .toArray
    Image(360, 180, pixels)
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    def centralAngle(loc1: Location, loc2: Location) = {
      val l1Lat = loc1.lat * PI / 180
      val l1Lon = loc1.lon * PI / 180
      val l2Lat = loc2.lat * PI / 180
      val l2Lon = loc2.lon * PI / 180
      if (loc1.equals(loc2)) 0.0
      else if (loc1.lat == -loc2.lat && (loc1.lon == loc2.lon + 180 || loc1.lon == loc2.lon - 180)) PI
      else acos(sin(l1Lat) * sin(l2Lat) + cos(l1Lat) * cos(l2Lat) * cos(abs(l1Lon - l2Lon)))
      new ConcurrentHashMap[]()

    }

    def greatCircleDistance(loc1: Location, loc2: Location): Double = centralAngle(loc1, loc2) * EARTH_RADIUS

    def interpolate(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
      val weightsAndTemps = temperatures.map(tuple => (1 / pow(greatCircleDistance(tuple._1, location), POWER_PARAMETER), tuple._2))
      weightsAndTemps.map(tuple => tuple._1 * tuple._2).sum / weightsAndTemps.map(_._1).sum
    }

    def inverseDistanceWeightingOpt(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
      val iterator = temperatures.iterator
      var denominator = 0.0
      var numerator = 0.0
      while (iterator.hasNext) {
        val tuple = iterator.next()
        val distance = greatCircleDistance(tuple._1, location)
        if (distance < 1) return tuple._2
        else {
          val weight = 1 / pow(distance, POWER_PARAMETER)
          numerator = numerator + weight * tuple._2
          denominator = denominator + weight
        }
      }
      numerator / denominator
    }

    def inverseDistanceWeighting(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature =
      temperatures.find(tuple => greatCircleDistance(tuple._1, location) < 1)
        .map(_._2)
        .getOrElse(interpolate(temperatures, location))

    inverseDistanceWeightingOpt(temperatures, location)
  }

  private def interpolateColorSorted(sortedPoints: Seq[(Temperature, Color)], value: Temperature): Color = {
    def interpolateShade(fromShade: Int, toShade: Int, fromT: Temperature, toT: Temperature, t: Temperature): Int = {
      Math.round(fromShade + (t - fromT) * (toShade - fromShade) / (toT - fromT)).toInt
    }

    val beginIndex = sortedPoints.lastIndexWhere(_._1 < value)
    if (beginIndex == sortedPoints.size - 1) sortedPoints(beginIndex)._2
    else if (beginIndex == -1) sortedPoints.head._2
    else {
      val begin = sortedPoints(beginIndex)
      val end = sortedPoints(beginIndex + 1)
      Color(interpolateShade(begin._2.red, end._2.red, begin._1, end._1, value),
        interpolateShade(begin._2.green, end._2.green, begin._1, end._1, value),
        interpolateShade(begin._2.blue, end._2.blue, begin._1, end._1, value)
      )
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color =
    interpolateColorSorted(points.toSeq.sortBy(_._1), value)

}

