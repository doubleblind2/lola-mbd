import scala.io.Source

object Averager {
  def main(args: Array[String]): Unit = {
    val colIndex = args(0).toInt
    val lines = Source.stdin.getLines().toList
    val values = lines.map { line =>
      line.split(",")(colIndex).trim.toDouble
    }
    val avgNs = values.sum / values.size
    val avgMs = avgNs / 1000000.0

    println(f"$avgMs%.4f ms")
  }
}
