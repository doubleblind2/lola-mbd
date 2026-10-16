package unaa_lola.util

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}

/**
 * Object containing logging stuff
 */
object Logging {

  /**
   * Constants and common function for Logging
   */
  object LoggingUtils {
    def sevToString(s: SEVERITY): String = s match {
      case MESSAGE => "Message:"
      case INFO => "Info:"
      case WARNING => "Warning:"
      case ERROR => "Error:"
      case FATAL => "Fatal:"
    }

    /**
     * Get string representation of channel
     * @param c Channel
     * @return String representation of channel
     */
    def channelToString(c: CHANNEL): String = c match {
      case TECHNICAL_LOG => "Technical Log"
      case RUNTIMES => "Runtime Log"
      case DEBUG => "Debug"
      case DIAGNOSES => "Diagnoses"
      case DIAG_RUNTIMES => "Diagnosis Runtimes"
    }

    /**
     * Extract stack trace as newline separated string from Throwable
     * @param t Throwable
     * @return Stack trace as string
     */
    def stackTraceFromThrowable(t: Throwable): String = {
      import java.io.PrintWriter
      import java.io.StringWriter

      val sw = new StringWriter
      t.printStackTrace(new PrintWriter(sw))
      sw.toString
    }

    /**
     * Convert time instant to string in format: yyyy-MM-dd HH:mm:ss.n
     * @param time Time
     * @return time instant as string
     */
    def timeString(time: Instant) : String = {
      val dt = LocalDateTime.ofInstant(time, ZoneId.systemDefault())
      dt.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.n"))
    }
  }

  /**
   * Use to globally switsch off logging (for runtime reasons etc)
   */
  private val LOGGING_ACTIVE = true

  /**
   * Enumeration of channels.
   * Channels are intended for logging different things in different formats or to different sinks.
   */
  sealed trait CHANNEL
  case object TECHNICAL_LOG extends CHANNEL
  case object RUNTIMES extends CHANNEL
  case object DEBUG extends CHANNEL
  case object DIAGNOSES extends CHANNEL
  case object DIAG_RUNTIMES extends CHANNEL

  /**
   * Enumeration of severity levels
   */
  sealed trait SEVERITY;
  case object MESSAGE extends SEVERITY
  case object INFO extends SEVERITY
  case object WARNING extends SEVERITY
  case object ERROR extends SEVERITY
  case object FATAL extends SEVERITY


  /**
   * Entry to be logged
   * @param time Time of log event
   * @param msg Message of log event
   * @param sev Severity level
   * @param channel Channel of log event
   * @param addInfo Additional information to be provided (optional)
   * @param throwable Throwable connected with the log entry (optional)
   */
  case class LogEntry(time: Instant, msg: String, sev: SEVERITY, channel: CHANNEL, addInfo: Option[String] = None, throwable: Option[Throwable] = None) {

    /** @inheritdoc */
    override def toString: String = {
      val ai = if (addInfo.isDefined) s"Additional Info: ${addInfo.get}" else ""
      val th = (if (ai == "") "" else " | ") +
               (if (throwable.isDefined) s"Exception thrown: ${throwable.get.getMessage}\n${LoggingUtils.stackTraceFromThrowable(throwable.get)}" else "")
      s"${LoggingUtils.timeString(time)} | $channel | ${LoggingUtils.sevToString(sev)} $msg | $ai $th"
    }
  }

  /**
   * Logging sink (following observer pattern)
   */
  abstract class LoggingSink {

    /**
     * Log a new entry
     * @param ent Entry to be (potentially) logged
     */
    def log(ent: LogEntry) : Unit
  }

  /**
   * Logger printing to stdout in human readable format
   * @param channels Set of channels to be logged
   * @param onlyMsg Print only messages of log entries
   */
  class StdIOLogger(channels: Set[CHANNEL], onlyMsg: Boolean = false) extends LoggingSink {

    /** @inheritdoc */
    override def log(ent: LogEntry): Unit = if (channels.contains(ent.channel)) System.err.println(if (onlyMsg) ent.msg else ent)
  }

  /**
   * Logger printing to a file in human readable format
   * @param channels Set of channels to be logged
   * @param path Path of log file
   * @param onlyMsg Print only messages of log entries
   */
  class FileLogger(channels: Set[CHANNEL], path: String, onlyMsg: Boolean = false) extends LoggingSink {
    import java.io._
    private val pw = new PrintWriter(new File(path))

    /** @inheritdoc */
    override def log(ent: LogEntry): Unit = if (channels.contains(ent.channel)) {
      pw.println(if (onlyMsg) ent.msg else ent)
      pw.flush()
    }

    /**
     * Close the involved log file (remember to unregister logger after calling!)
     */
    def close() : Unit = pw.close()
  }

  private val sinks : collection.mutable.Set[LoggingSink] = collection.mutable.Set()

  /**
   * Add sink for logging
   * @param s Sink to be added
   */
  def addSink(s: LoggingSink): Unit = {
    sinks += s
  }

  /**
   * Remove sink for logging
   * @param s Sink to be removed
   */
  def remSink(s: LoggingSink): Unit = {
    sinks -= s
  }

  /**
   * Remove all active sinks
   */
  def clearSinks(): Unit = {
    sinks.clear()
  }

  /**
   * Log an event
   * @param msg Message of log event
   * @param sev Severity level
   * @param addInfo Additional information to be provided (optional)
   * @param throwable Throwable connected with the log entry (optional)
   * @param channel Channel of log event
   */
  def log(msg: String, sev: SEVERITY = INFO, addInfo: Option[String] = None, throwable: Option[Throwable] = None, channel: CHANNEL = TECHNICAL_LOG): Unit = {
    if (LOGGING_ACTIVE) {
      val entry = LogEntry(Instant.now(), msg, sev, channel, addInfo, throwable)
      sinks.foreach(_.log(entry))
    }
  }
}
