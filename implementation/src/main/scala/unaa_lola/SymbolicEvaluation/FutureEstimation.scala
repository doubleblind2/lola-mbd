package unaa_lola.SymbolicEvaluation

import unaa_lola.AST._
import unaa_lola.util.{Errors, Logging}
import unaa_lola.Implementations
import unaa_lola.Config


/**
  * A Future is a collection of states that were precomputed to infer values of positive offsets.
  *
  * @param states A sequence of all states describing the future, all states are 0-indexed
  * @param loopTo Marks to which state the last state in the states sequence loops to. If this is -1, no loop has been found, otherwise between 0 and states.length - 1
  * @param maxTs The maximum timestamp
  */
case class Future(states: Seq[ConstraintState], loopTo: Int, maxTs: Option[Int]) {

  /**
    * Determines the future state for a specific timestamp.
    *
    * @param ts The timestamp of the desired state
    * @return The future state for ts
    */
  def getStateEstimation(ts: Int): Option[ConstraintState] = {
    if (maxTs.isEmpty || (loopTo == -1 && maxTs.get - ts > states.length)) {
      None
    } else {
      val pfx = loopTo
      val loopLen = states.length - loopTo
      val cs = if (ts >= maxTs.get - pfx) {
        states(maxTs.get - ts).setTs(ts)
      } else {
        val posInSeq = pfx + (maxTs.get - pfx - ts) % loopLen
        states(posInSeq).setTs(ts = ts)
      }
      Some(cs)
    }
  }

  override def toString(): String = {
    var out = states.zipWithIndex.foldLeft("Future states (starting from last timestamp):\n") {
      case (s, (state, i)) =>
        s + s"$i:\n${state.toString}\n"
    }
    out + s"Last state loops to state $loopTo\n\n"
  }

}

/**
  * Builder for [[Future]]s.
  *
  * @param impl The concrete implementation
  * @param config A configuration
  * @param flatspec The flattened spec
  */
class FutureEstimation(impl: Implementations.Implementation, config: Config, flatspec: Specification, simplificationStrat: SimplificationStrategy) {

  // Sets of stream identifiers (input + non-input) with specific timestamps.
  private val identifierZero = flatspec.identifiers.map(SymbolIdentifier(_, 0))
  private val identifierPlus1 = flatspec.identifiers.map(SymbolIdentifier(_, 1))

  // Set of all +1-refernced identifiers in stream definitions of the spec timestamped with 0.
  private val POneUsedZero = flatspec.crawlOffsets(1).map(SymbolIdentifier(_, 0))

  // Identifiers that were erased during simplification and cleanup. 
  // Must be ignored during congruency check if parts of the concrete implementation does not correctly eliminate them there.
  private val ignoreCongruence = identifierPlus1 ++ (identifierZero -- POneUsedZero)

  /**
    * Constructs a [[Future]] for the given implementation and spec.
    *
    * @param maxSteps Maximum amount of steps to be done before finding a loop
    * @return The future
    */
  def constructFuture(maxSteps: Option[Int]): Future = {
    val steps = Math.min(maxSteps.getOrElse(Int.MaxValue), config.maxTs.getOrElse(Int.MaxValue))
    var cs = ConstraintState.getEmptyState(flatspec, impl, config).transformTs(config.maxTs.getOrElse(Int.MaxValue - 1) + 1)
    val transformer = new ConstraintStateTransformer(flatspec, simplificationStrat, config)
    var csSeq: Seq[ConstraintState] = Seq()

    var loopTo: Int = -1
    var i = 0
    while(i < steps && loopTo == -1) {
      cs = transformer.computePredecessor(cs)
      
      loopTo = csSeq.indexWhere(cs.isCongruentTo(_, ignore = ignoreCongruence))
      if (loopTo == -1) {
        csSeq :+= cs
        Logging.log(s"Future state number $i computed.", channel = Logging.TECHNICAL_LOG)
      }
      
      i += 1

      if (i >= steps) {
        Logging.log(s"Future estimation could not be determined in $steps steps. Giving up!", sev = Logging.WARNING)
      }
    }

    Future(csSeq, loopTo, config.maxTs)
  }

}