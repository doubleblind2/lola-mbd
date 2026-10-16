import scala.io.Source
import java.nio.file.{Paths, Files}
import scala.util.Try

object MaximumRuntime {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      println("Usage: scala MaxRuntimeFinderNoT <c> <column_index>")
      println("Example: scala MaxRuntimeFinderNoT test1 1")
      println("Note: column_index is 0-based (0 = first column, 1 = second column)")
      System.exit(1)
    }

    val c = args(0)

    val columnIndex = Try(args(1).toInt).getOrElse(-1)

    if (columnIndex < 0) {
      println("Error: Column index must be a non-negative integer.")
      System.exit(1)
    }

    var globalMaxNs: Long = Long.MinValue
    var maxK: Int = -1
    var maxLineNum: Int = -1

    var filesProcessed = 0

    for (k <- 0 to 31) {
      val filePath = Paths.get("results", c, k.toString, "diagnosis_runtime.log")

      if (Files.exists(filePath)) {
        var source: Source = null
        try {
          source = Source.fromFile(filePath.toFile)

          for ((line, index) <- source.getLines().zipWithIndex) {
            val parts = line.split(",")

            if (parts.length > columnIndex) {
              val currentVal = Try(parts(columnIndex).trim.toLong).getOrElse(Long.MinValue)

              if (currentVal > globalMaxNs) {
                globalMaxNs = currentVal
                maxK = k
                maxLineNum = index + 1
              }
            }
          }
          filesProcessed += 1

        } catch {
          case e: Exception =>
            println(s"Warning: Could not read file $filePath: ${e.getMessage}")
        } finally {
          if (source != null) source.close()
        }
      }
    }

    // 4. Output results
    if (filesProcessed == 0) {
      println(s"No logs found for configuration c=$c")
    } else if (globalMaxNs == Long.MinValue) {
      println(s"Files processed, but no valid numeric data found in column $columnIndex.")
    } else {
      val maxMs = globalMaxNs / 1000000.0

      println("--------------------------------------------------")
      println(f"Processed $filesProcessed files.")
      println(f"Maximum Value (Column Index $columnIndex): $maxMs%.4f ms")
      println("Location:")
      println(f"  k           : $maxK")
      println(f"  Line number : $maxLineNum")
      println("--------------------------------------------------")
    }
  }
}
