package unaa_lola.Frontend

import org.antlr.v4.runtime.{BaseErrorListener, Recognizer, RecognitionException}
import org.antlr.v4.runtime.misc.ParseCancellationException


/**
  * ANTLR error listener used during lexing and parsing that raises an error immediately.
  *
  */
class ThrowingErrorListener extends BaseErrorListener {
  
  override def syntaxError(Recognizer: Recognizer[_, _], offendingSymbol: Object, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
    throw LolaSyntaxException(Location(line, charPositionInLine), msg)
  }

}

/**
  * ANTLR error listener used during lexing and parsing that does not raise an error immediately but collects them.
  *
  */
class CollectingErrorListener extends BaseErrorListener {

  private var exceptionList: Seq[LolaCompilerException] = Seq()

  /**
    * Raise every collected syntax error at once.
    */
  def finish() = if (exceptionList.nonEmpty) throw LolaMultiCompilerException(exceptionList)
  
  override def syntaxError(Recognizer: Recognizer[_, _], offendingSymbol: Object, line: Int, charPositionInLine: Int, msg: String, e: RecognitionException): Unit = {
    exceptionList :+= LolaSyntaxException(Location(line, charPositionInLine), msg)
  }

}

/**
  * Common type for exceptions during compilation.
  */
sealed trait LolaCompilerException extends Exception

/**
  * Exception used for syntactic errors raised during ANTLR's parsing procedure.
  * Will cancel the parsing upon raising.
  *
  * @param location The locaton of the error
  * @param msg Message describing the reason of the exception
  */
case class LolaSyntaxException(location: Location, msg: String) extends ParseCancellationException(s"line $location $msg") with LolaCompilerException {
  override def toString = s"Syntax Error: line $location $msg"
}

/**
  * Exception used for compiler errors concerning types, e.g. type mismatches.
  *
  * @param location The locaton of the error
  * @param msg Message describing the reason of the exception
  */
case class LolaTypeException(location: Location, msg: String) extends LolaCompilerException {
  override def toString = s"Type Error: line $location $msg"
}

/**
  * Exception used for compiler errors concerning the declaration of streams and enums, e.g. referencing a stream that is not yet defined.
  *
  * @param location The locaton of the error
  * @param msg Message describing the reason of the exception
  */
case class LolaDeclarationException(location: Location, msg: String) extends LolaCompilerException {
  override def toString = s"Declaration Error: line $location $msg"
}

/**
  * Container for a sequence of compiler exceptions.
  * Used for collecting multiple errors before raising them.
  *
  * @param exceptions The sequence of exceptions
  */
case class LolaMultiCompilerException(exceptions: Seq[LolaCompilerException]) extends LolaCompilerException {
  override def toString: String = exceptions.map(_.toString).mkString("\n")
}

/**
  * Specifies the location of an error.
  *
  * @param line The line of the error
  * @param column The column of the error
  */
case class Location(line: Int, column: Int) {
  override def toString = s"$line: $column"
}
