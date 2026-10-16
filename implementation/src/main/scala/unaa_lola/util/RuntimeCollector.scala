package unaa_lola.util

import unaa_lola.Config

/**
  * Collects runtimes specific to certain portions of the code
  */
class RuntimeCollector {

  private var total: Long = 0
  private var currentStart: Long = 0

  /**
    * Starts measuring a new code portion
    */
  def start(): Unit = {
    if (currentStart != 0) {
      Logging.log("RuntimeCollector started new meassurement before previous one was completed.", sev=Logging.WARNING)
    } else {
      currentStart = System.nanoTime()
    }
  }

  /**
    * Stops measuring the current code portion and adds the measured runtime to the collected sum
    */
  def stop(): Unit = {
    total += System.nanoTime() - currentStart
    currentStart = 0
  }

  /**
    * Returns the collected runtime
    *
    * @return the collected runtime in microseconds
    */
  def getTotalTimeInMicro = total / 1000

  /**
    * Resets the collected runtime sum to 0
    */
  def reset(): Unit = {
    total = 0
    currentStart = 0
  }

}