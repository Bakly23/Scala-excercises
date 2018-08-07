package observatory

import java.time.LocalDate

import org.scalatest.FunSuite

trait ExtractionTest extends FunSuite {
  test("locating temperatures sunny day test") {
    val temperatures = Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
    assert(temperatures.size == 16)
    assert(!temperatures.exists(tuple => tuple._2.lat === +78.917 && tuple._2.lon === +011.933))
  }

  test("defining average records sunny day test") {
    val avgRecords = Extraction.locationYearlyAverageRecords(Seq(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.0)
    ))
    assertResult(Seq(
      (Location(37.358, -78.438), 1.0),
      (Location(37.35, -78.433), 27.3)
    ).toList)(avgRecords.toList.sortBy(_._2))
  }

  test("combined test of extraction and averagin") {
    val temperatures = Extraction.locateTemperatures(1975, "/stations.csv", "/1975.csv")
    val avgRecords = Extraction.locationYearlyAverageRecords(temperatures)
    assertResult(Seq(
      (Location(+78.067,+013.633), -19.007936507936506),
      (Location(+70.933,-008.667), -7.450617283950617)
    ).toList)(avgRecords.toList.sortBy(_._2))
  }

}