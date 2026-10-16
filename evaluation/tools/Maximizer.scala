import scala.io.Source

object Maximizer {
  def main(args: Array[String]): Unit = {
    val colIndex = args(0).toInt
    val values = Source.stdin.getLines().map { line =>
      line.split(",")(colIndex).trim.toDouble
    }.toList
    val maxNs = values.max
    val maxMs = maxNs / 1000000.0

    println(f"$maxMs%.4f ms")
  }
}
