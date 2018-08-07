package observatory

import java.io.InputStream
import java.time.LocalDate

import org.apache.spark.sql.types.DataTypes._
import org.apache.spark.sql.types.{StructField, StructType}
import org.apache.spark.sql.{Dataset, Row, SparkSession}

/**
  * 1st milestone: data extraction
  */
object Extraction {
  val spark = SparkSession.builder.appName("Locating Temperature").master("local[*]").getOrCreate
  spark.sparkContext.setLogLevel("WARN")

  import spark.implicits._

  val tStruct = StructType(List(
    StructField("stn", LongType),
    StructField("wban", LongType),
    StructField("month", IntegerType),
    StructField("day", IntegerType),
    StructField("temperature", DoubleType)))
  val sStruct = StructType(List(
    StructField("stn", LongType),
    StructField("wban", LongType),
    StructField("lat", DoubleType),
    StructField("long", DoubleType)))


  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    def parseLine(struct: StructType): String => Option[Row] = {
      str => {
        val tokens = str.split(",", struct.size)
        if (tokens.size == struct.size) {
          def emptyToNullParse[T >: Null](token: String, parser: String => T): T =
            if (token.isEmpty) null else parser(token)

          Option.apply(Row.fromSeq(tokens.zip(struct).map(tuple => tuple._2.dataType match {
            case LongType => emptyToNullParse(tuple._1, t => t.toLong)
            case DoubleType => emptyToNullParse(tuple._1, t => t.toDouble)
            case IntegerType => emptyToNullParse(tuple._1, t => t.toInt)
            case _ => tuple._1
          }).toSeq))
        } else if (tokens.nonEmpty) {
          val msg = s"invalid tokens: ${tokens.mkString(",")} for line '$str', size of tokens is ${tokens.length}, size of schema is ${struct.size}"
          println(msg)
          throw new RuntimeException(msg)
        } else {
          Option.empty
        }
      }
    }

    def readCsv(file: String, struct: StructType): Dataset[Row] = {
      val stream: InputStream = getClass.getResourceAsStream(file)
      val lines = scala.io.Source.fromInputStream(stream).getLines.toSeq
      spark.createDataFrame(spark.sparkContext.parallelize(lines)
        .map(parseLine(struct))
        .filter(_.isDefined)
        .map(_.get), struct)
    }

    val temperatures = readCsv(temperaturesFile, tStruct).filter($"temperature" =!= 9999)

    val stations = readCsv(stationsFile, sStruct).filter($"long".isNotNull || $"lat".isNotNull)

    def safeTakeInt(row: Row, field: String): Int = if (row.isNullAt(row.fieldIndex(field))) 0 else row.getAs[Int](field)

    temperatures.as("t").join(stations.as("s"), $"t.stn" <=> $"s.stn" && $"t.wban" <=> $"s.wban")
      .map(row => (year, row.getAs[Int]("month"), row.getAs[Int]("day"), Location(row.getAs("lat"), row.getAs("long")),
        (row.getAs[Double]("temperature") - 32) * 5.0 / 9))
      .collect()
      .map(t => (LocalDate.of(t._1, t._2, t._3), t._4, t._5))
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.groupBy(_._2)
      .mapValues(tups => tups.map(_._3))
      .mapValues(temps => temps.sum / temps.size)
  }

}
