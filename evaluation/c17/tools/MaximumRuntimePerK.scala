import scala.io.Source
import java.nio.file.{Paths, Files}
import scala.util.Try

object MaximumRuntimePerK {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage: scala MaxRuntimePerK <column_index>")
      println("Example: scala MaxRuntimePerK 1")
      println("Note: column_index is 0-based.")
      System.exit(1)
    }

    val columnIndex = Try(args(0).toInt).getOrElse(-1)

    if (columnIndex < 0) {
      println("Error: Column index must be a non-negative integer.")
      System.exit(1)
    }

    println(f"${"k"}|${"Max Time (ms)"}%-15s|${"Found in c"}|${"Line Num"}")
    println("-" * 45)

    for (k <- 0 to 31) {

      // State for the current k
      var maxNsForK: Long = Long.MinValue
      var bestC: Int = -1
      var bestLineNum: Int = -1
      var foundDataForK = false

      for (c <- 1 to 6) {
        val filePath = Paths.get("results", c.toString, k.toString, "diagnosis_runtime.log")

        if (Files.exists(filePath)) {
          var source: Source = null
          try {
            source = Source.fromFile(filePath.toFile)

            for ((line, index) <- source.getLines().zipWithIndex) {
              val parts = line.split(",")

              if (parts.length > columnIndex) {
                val currentVal = Try(parts(columnIndex).trim.toLong).getOrElse(Long.MinValue)

                if (currentVal > maxNsForK) {
                  maxNsForK = currentVal
                  bestC = c
                  bestLineNum = index + 1
                  foundDataForK = true
                }
              }
            }
          } catch {
            case e: Exception =>
              // println(s"Warning: Error reading $filePath: ${e.getMessage}")
          } finally {
            if (source != null) source.close()
          }
        }
      }

      // 4. Output results for this k
      if (foundDataForK) {
        val maxMs = maxNsForK / 1000000.0
        println(f"$k%d| $maxMs%-14.4f| $bestC%-9d| $bestLineNum")
      } else {
        println(f"$k%d| ${"No Data"}%-14s| ${"-"}%-9s| -")
      }
    }
  }
}
