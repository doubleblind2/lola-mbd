import scala.io.Source
import java.nio.file.{Paths, Files}
import scala.util.Try

object MaximumRuntimePerK {

  val comps = Seq("g5", "g6", "g7", "g9", "g10", "g12", "g15")

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Usage: scala MaxRuntimePerKWithT <column_index>")
      println("Example: scala MaxRuntimePerK 1")
      println("Note: column_index is 0-based.")
      System.exit(1)
    }

    val columnIndex = Try(args(0).toInt).getOrElse(-1)

    if (columnIndex < 0) {
      println("Error: Column index must be a non-negative integer.")
      System.exit(1)
    }

    println(f"${"k"}%-3s| ${"Max Time (ms)"}%-15s| ${"c"}%-3s| ${"t"}%-3s| ${"Line"}")
    println("-" * 50)

    for (k <- 0 to 9) {

      var maxNsForK: Long = Long.MinValue
      var bestC: String = ""
      var bestT: Int = -1
      var bestLineNum: Int = -1
      var foundData = false

      for (c <- comps) {
        for (t <- 1 to 10) {

           val filePath = Paths.get("results", c.toString, k.toString, t.toString, "diagnosis_runtime.log")

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
                     bestT = t
                     bestLineNum = index + 1
                     foundData = true
                   }
                 }
               }
             } catch {
               case e: Exception =>
             } finally {
               if (source != null) source.close()
             }
           }
        }
      }

      if (foundData) {
        val maxMs = maxNsForK / 1000000.0
        println(f"$k%-3d| $maxMs%-15.4f| $bestC| $bestT%-3d| $bestLineNum")
      } else {
        println(f"$k%-3d| ${"No Data"}%-15s| ${"-"}%-3s| ${"-"}%-3s| -")
      }
    }
  }
}
