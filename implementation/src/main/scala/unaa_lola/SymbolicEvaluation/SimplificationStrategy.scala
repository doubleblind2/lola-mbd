package unaa_lola.SymbolicEvaluation

/**
  * A SimplificationStrategy contains functions to influence simplification.
  */
abstract class SimplificationStrategy {

  /**
    * Proceeds the step of the strategy.
    */
  def proceedStep() : Unit

  /**
    * Decides whether simplification should occur.
    *
    * @return True if simplificaiton should occur. 
    */
  def optimizationNecessary: Boolean

  /**
    * Returns the current simplification stage, which decides the type of simplification.
    *
    * @return The stage
    */
  def optimizationStage: Int

}

/**
 * [[SimplificationStrategy]] which follows an automaton scheme passed by the transition table argument
 * @param transitionTable Maps a state to a tuple (b, s, n, t) where b indicates whether a simplification shall be
 *                        performed, s the stage the simplification shall be performed with, n the number of steps
 *                        the automaton shall remain in this state, t the successor state
 */
class SimplificationStrategyAutomaton(val transitionTable : Map[Int, (Boolean, Int, Int, Int)]) extends SimplificationStrategy {

  private var state : Int = 0;
  private var cnt : Int = 0;
  override def proceedStep(): Unit = {
    val (_, _, n, t) = transitionTable(state)

    if (cnt >= n-1) {
      state = t
      cnt = 0
    } else {
      cnt += 1
    }
  }

  override def optimizationNecessary: Boolean = {
    val (b, _, _, _) = transitionTable(state)
    b
  }

  override def optimizationStage: Int = {
    val (_, s, _, _) = transitionTable(state)
    s
  }
}

/**
  * Contains common strategies.
  */
object CommonStraegies {

  /**
    * No simplification at all.
    */
  val NoSimplificationStrategy: SimplificationStrategy = new SimplificationStrategyAutomaton(
    Map(
      0 -> (false, 0, 0, 0)
    )
  )

  /**
    * Simplify in certain time intervals.
    *
    * @param interval The interval between two simplifications
    */
  def IntervalSimplificationStrategy(interval: Int): SimplificationStrategy = new SimplificationStrategyAutomaton(
    if (interval == 1) {
      Map(0 -> (true, 1, 0, 0))
    } else {
      Map(
        0 -> (false, 0, interval - 1, 1),
        1 -> (true, 1, 1, 0)
      )
    }
  )

  def IntervalSimplificationStrategyStrong(interval: Int): SimplificationStrategy = new SimplificationStrategyAutomaton(
    if (interval == 1) {
      Map(0 -> (true, 4, 0, 0))
    } else {
      Map(
        0 -> (false, 0, interval - 1, 1),
        1 -> (true, 4, 1, 0)
      )
    }
  )

  /**
    * General strategy for future simplification. Each argument defines how long the automaton stays in one stage before advancing to the next. 
    * Will do one simplifying-free step between each stage.
    */
  def FutureSimplificationStrategy(j: Int, k: Int, l: Int, m: Int, n: Int, o: Int, p: Int): SimplificationStrategy = new SimplificationStrategyAutomaton(
    Map(
      0 -> (false, 0, j, 1),
      1 -> (true, 1, 0, 2),
      2 -> (false, 0, k, 3),
      3 -> (true, 2, 0, 4),
      4 -> (false, 0, l, 5),
      5 -> (true, 3, 0, 6),
      6 -> (false, 0, m, 7),
      7 -> (true, 4, 1, 8),
      8 -> (false, 0, n, 9),
      9 -> (true, 5, 1, 10),
      10 -> (false, 0, o, 11),
      11 -> (true, 6, 1, 12),
      12 -> (false, 0, p, 13),
      13 -> (true, 6, 1, 13)
    )
  )
  val StandardFutureSimplificationStrategy: SimplificationStrategy = FutureSimplificationStrategy(10, 10, 10, 10, 10, 10, 10)

  /**
    * Starts widening after a certain number of steps. Does not simplfy before.
    *
    * @param after When widening shall begin to be applied.
    */
  def WideningOnlySimplificationStrategy(after: Int) = new SimplificationStrategyAutomaton(
    Map(
      0 -> (false, 0, after, 1),
      1 -> (true, 5, 1, 1)
    )
  )
}