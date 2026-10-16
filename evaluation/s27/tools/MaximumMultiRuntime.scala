import scala.io.Source
import java.nio.file.{Paths, Files}
import scala.util.Try

object MaximumRuntime {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      println("Usage: scala MaxRuntimeFinder <c> <column_index>")
      println("Example: scala MaxRuntimeFinder test1 1")
      println("Note: column_index is 0-based (0 = first column, 1 = second column)")
      System.exit(1)
    }

    val c = args(0)

    val columnIndex = Try(args(1).toInt).getOrElse(-1)

    if (columnIndex < 0) {
      println("Error: Column index must be a non-negative integer (0, 1, 2...).")
      System.exit(1)
    }

    var globalMaxNs: Long = Long.MinValue
    var maxK: Int = -1
    var maxT: Int = -1
    var maxLineNum: Int = -1

    var filesProcessed = 0

    for {
      k <- 0 to 9
      t <- 1 to 10
    } {
      val filePath = Paths.get("results", c, k.toString, t.toString, "diagnosis_runtime.log")

      if (Files.exists(filePath)) {
        var source: Source = null
        try {
          source = Source.fromFile(filePath.toFile)

          // 3. Iterate through lines with their index
          for ((line, index) <- source.getLines().zipWithIndex) {
            val parts = line.split(",")

            // Check if the requested column index exists in this line
            if (parts.length > columnIndex) {
              // Parse the value at the specified column safely
              val currentVal = Try(parts(columnIndex).trim.toLong).getOrElse(Long.MinValue)

              if (currentVal > globalMaxNs) {
                globalMaxNs = currentVal
                maxK = k
                maxT = t
                maxLineNum = index + 1 // Keep line number 1-based for human readability
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
      println(f"  t           : $maxT")
      println(f"  Line number : $maxLineNum")
      println("--------------------------------------------------")
    }
  }
}
