package unaa_lola.util

import unaa_lola.AST
import unaa_lola.Value.Bool

/**
 * Object with useful common functions
 */
object Common {

  /**
   * Type for interval with options.
   * None is used if bound is not known.
   */
  type OInterval = (Option[Double], Option[Double])

  /**
    * Stream identifier for assumption stream
    */
  val assStream = AST.Identifier("ASSUMPTION", Bool)

  /**
    * Checks if an interval contains a real number
    *
    * @param i The interval
    * @param d The real number
    * @return true if i contains d
    */
  def intervalContains(i: OInterval, d: Double): Boolean = (i._1.isEmpty || i._1.get <= d) && (i._2.isEmpty || i._2.get >= d)

  /**
   * Creates the intersection of two intervals
   * @param i1 First interval
   * @param i2 Second interval
   * @return The intersection of intervals
   */
  def intersectIntervals(i1: OInterval, i2: OInterval): OInterval = {
    (
      if (i1._1.isEmpty) i2._1 else if (i2._1.isEmpty) i1._1 else if (i1._1.get > i2._1.get) i1._1 else i2._1,
      if (i1._2.isEmpty) i2._2 else if (i2._2.isEmpty) i1._2 else if (i1._2.get < i2._2.get) i1._2 else i2._2
    )
  }

  /**
    * Creates the union of two intervals
    *
    * @param i1 First interval
    * @param i2 Second interval
    * @return The union of the intervals
    */
  def unionIntervals(i1: OInterval, i2: OInterval): OInterval = {
    (
      if (i1._1.isEmpty || i2._1.isEmpty) None else Some(Math.min(i1._1.get, i2._1.get)),
      if (i1._2.isEmpty || i2._2.isEmpty) None else Some(Math.max(i1._2.get, i2._2.get))
    )
  }

  /**
    * Applies the widening operator between two intervals.
    * If the second interval has a smaller lower bound or larger upper bound,
    * the new bound is set to -infinty or infinity, respectively.
    * 
    * @param i1 The left interval
    * @param i2 The right interval
    * @return The left interval widened by the right interval
    */
  def widenIntervals(i1: OInterval, i2: OInterval): OInterval = {
    val (l1, r1) = i1
    val (l2, r2) = i2

    (
      if (l1.isDefined && l2.isDefined && l1.get <= l2.get) Some(l1.get) else None,
      if (r1.isDefined && r2.isDefined && r2.get <= r1.get) Some(r1.get) else None
    )
  }

}
