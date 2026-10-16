import scala.io.Source
import java.io.File

object MultiDiagnosisComparator {

  def main(args: Array[String]): Unit = {
    val baseDir = "results"

    for (k <- 0 to 30) {
      val fileK = new File(baseDir + "/" + k + "/diagnoses.log")
      val fileNext = new File(baseDir + "/" + (k + 1) + "/diagnoses.log")

      if (fileK.exists() && fileNext.exists()) {
        val multiLinesK = getMultiData(fileK)
        val multiLinesNext = getMultiData(fileNext)

        println(f"=== Comparing Folder $k%2d with Folder ${k + 1}%2d (Strict Supersets) ===")

        var totalCoverageSum = 0.0
        val timestampCount = multiLinesK.size

        multiLinesK.zip(multiLinesNext).foreach { case ((tK, setsK), (tNext, setsNext)) =>
          if (tK == tNext) {
            val coveredCount = setsK.count { sk =>
              setsNext.exists(sn => sk.subsetOf(sn) && sk != sn)
            }

            val coveragePercent = if (setsK.nonEmpty) {
              (coveredCount.toDouble / setsK.size) * 100
            } else 0.0

            totalCoverageSum += coveragePercent
            println(f"t=$tK%2s: $coveragePercent%6.2f%% strictly replaced ($coveredCount/${setsK.size} diagnoses)")
          }
        }

        if (timestampCount > 0) {
          val avgCoverage = totalCoverageSum / timestampCount
          println(f"--- Average Strict Replacement for Folder $k: $avgCoverage%.2f%% ---")
        }
        println()
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

          val diagnoses = if (rawSets == "{{}}" || rawSets.trim.isEmpty || rawSets == "{{ }}") {
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
