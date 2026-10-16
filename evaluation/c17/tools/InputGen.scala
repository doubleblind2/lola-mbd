import scala.util.Random

object InputGen {

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Usage: scala InputGen <n>")
      System.exit(1)
    }

    val n = args(0).toInt
    val rand = new Random

    println("i1,i2,i3,i4,i5")

    for (_ <- 1 to n) {
      val row = Seq.fill(5)(rand.nextBoolean).mkString(",")
      println(row)
    }
  }
}
