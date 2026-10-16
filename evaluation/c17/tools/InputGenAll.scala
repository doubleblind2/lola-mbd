 
object InputGenAll {

  def main(args: Array[String]): Unit = {
    println("i1,i2,i3,i4,i5")

    for (i <- 0 until 32) {

      val row = (4 to 0 by -1)
        .map(bit => ((i >> bit) & 1) == 1)
        .mkString(",")

      println(row)

    }

  }

}