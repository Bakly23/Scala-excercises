package observatory


import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers {
  test("predicting temperature test") {

  }

  test ("interpolating color test") {
    val tempsToColors = List(
      (-60.0, Color(0, 0, 0)),
      (60.0, Color(255, 255, 255))
    )
    val testScale = List((0.0,Color(255,0,0)), (2.147483647E9,Color(0,0,255)))
    assert(Color(128, 0, 128).equals(Visualization.interpolateColor(testScale, 1.0737418235E9)))
    assert(Color(255, 255, 255).equals(Visualization.interpolateColor(tempsToColors, 61)))
    assert(Color(0, 0, 0).equals(Visualization.interpolateColor(tempsToColors, -61)))
  }


}
