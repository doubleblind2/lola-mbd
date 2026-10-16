import scala.io.Source
import java.io.File

object MultiDiagnosisComparator31 {

  def main(args: Array[String]): Unit = {
    val baseDir = "results"
    val c = args(0).toInt
    val file31 = new File(baseDir + s"/$c/31/diagnoses.log")
    if (!file31.exists()) return
    val data31 = getMultiData(file31)

    for (k <- 0 to 30) {
      val fileK = new File(baseDir + s"/$c/$k/diagnoses.log")

      if (fileK.exists()) {
        val dataK = getMultiData(fileK)
        var totalCoverageSum = 0.0

        dataK.zip(data31).foreach { case ((_, setsK), (_, sets31)) =>
          val coveredCount = setsK.count { sk =>
            sets31.exists(s31 => sk.subsetOf(s31) && sk != s31)
          }
          if (setsK.nonEmpty) {
            totalCoverageSum += (coveredCount.toDouble / setsK.size) * 100
          }
        }

        val avgCoverage = if (dataK.nonEmpty) totalCoverageSum / dataK.size else 0.0
        println(f"$k: $avgCoverage%.0f%%")
      }
    }
  }

  def getMultiData(file: File): List[(String, Set[Set[String]])] = {
    var source: Source = null
    try {
      source = Source.fromFile(file)
      source.getLines().flatMap { line =>
        val parts = line.split(",", -1)
        if (parts.length >= 4 && parts(1).trim == "Multi") {
          val timestamp = parts(0).trim
          val rawSets = parts(3).trim

          val diagnoses = if (rawSets == "{{}}" || rawSets.trim.isEmpty) {
            Set(Set.empty[String])
          } else {
            val inner = rawSets.stripPrefix("{{").stripSuffix("}}")
            inner.split("\\},\\{").map { s =>
              s.replace("{", "").replace("}", "")
               .split(",")
               .map(_.trim)
               .filter(_.nonEmpty)
               .toSet
            }.toSet.filter(_.nonEmpty)
          }
          Some(timestamp -> diagnoses)
        } else None
      }.toList
    } catch {
      case e: Exception => Nil
    } finally {
      if (source != null) source.close()
    }
  }
}
