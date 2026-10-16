package unaa_lola.util

import com.microsoft.z3.Status

// TODO think about removing panic altogether

/**
 * Common object containing errors
 */
object Errors {

  /**
   * Container exception to be raised if exception is thrown at unexpected place
   * @param t Unexpected exception
   * @param panic Panic required
   */
  class UnexpectedException(t: Throwable, panic: Boolean = false) extends Throwable {
    Logging.log("Exception thrown", if (panic) Logging.FATAL else Logging.ERROR, None, Some(t))

    if (panic) {
      System.err.println("Exception with panic instruction occurred!")
      System.err.println(s"$toString")
    }

    /** @inheritdoc */
    override def toString = s"Unexpected exception: ${t.toString}"
  }


  /**
   * General exception class for intended exceptions in the code
   * @param msg Message describing the reason of the exception
   * @param panic Panic required
   * @param debugOnly Only log when debugging is enabled
   */
  class UnAALOLAException(msg: String, panic: Boolean = false, debugOnly: Boolean = false) extends Throwable {
    Logging.log(s"Exception thrown: $msg", if (panic) Logging.FATAL else Logging.ERROR, channel = if (debugOnly) Logging.DEBUG else Logging.TECHNICAL_LOG)

    if (panic) {
      System.err.println("Exception with panic instruction occurred!")
      System.err.println(s"$toString")
    }

    /** @inheritdoc */
    override def toString = s"Common exception: $msg"
  }

  /**
   * Exception class that is due to internal reasons rather than caused by the user
   * @param msg Message describing the reason of the exception
   * @param panic Panic required
   */
  class InternalException(msg: String, panic: Boolean = false) extends UnAALOLAException(msg, panic)

  /**
   * Exception connected with z3
   * @param msg Message describing the reason of the exception
   * @param panic Panic required
   * @param debugOnly Only log when debugging is enabled
   */
  class Z3Exception(msg: String, panic: Boolean = false, debugOnly: Boolean = false) extends UnAALOLAException(msg, panic, debugOnly) {

    /** @inheritdoc */
    override def toString = s"z3 exception: $msg"
  }

  /**
   * Special exception for problems during z3 optimization
   * @param s z3 status of optimization
   * @param reason Reason of exception as string
   * @param panic Panic required (will lead to crash)
   * @param debugOnly Only log when debugging is enabled
   */
  class Z3OptimizationException(s: Status, reason: String, panic: Boolean = false, debugOnly: Boolean = false) extends Z3Exception(s"Error with optimization: $s, Reason: $reason", panic, debugOnly)

  case object EndOfInputReachedException extends UnAALOLAException("End of input reached", false, false)
}