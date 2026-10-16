import scala.io.Source
import java.io.File

object ContainCheck {

  def main(args: Array[String]): Unit = {
    val baseDir = "results"

    for (k <- 0 to 9) {
      for (t <- 1 to 10) {
        val filePath = baseDir + s"/${args(0)}/$k/$t/diagnoses.log"
        val file = new File(filePath)

        if (!file.exists()) {
          println(f"Folder ${args(0)}/$k%2d/$t%2d: [MISSING] File not found at $filePath")
        } else {
          validateFile(file) match {
            case Right(Nil) =>
              println(f"Folder ${args(0)}/$k%2d/$t%2d: [OK] All lines valid.")
            case Right(errors) =>
              println(f"Folder ${args(0)}/$k%2d/$t%2d: [FAIL] Invalid data on lines: ${errors.mkString(", ")}")
            case Left(errorMessage) =>
              println(f"Folder ${args(0)}/$k%2d/$t%2d: [ERROR] $errorMessage")
          }
        }
      }
    }
  }

  def validateFile(file: File): Either[String, List[Int]] = {
    var source: Source = null
    try {
      source = Source.fromFile(file)
      val invalidLines = source.getLines().zipWithIndex.flatMap { case (line, index) =>
        val parts = line.split(",", -1)

        val isInvalid = if (parts.length >= 3) {
          val col3 = parts(2).trim
          col3.nonEmpty && !col3.contains("=true")
        } else {
          true
        }

        if (isInvalid) Some(index + 1) else None
      }.toList

      Right(invalidLines)
    } catch {
      case e: Exception => Left(e.getMessage)
    } finally {
      if (source != null) source.close()
    }
  }
}
