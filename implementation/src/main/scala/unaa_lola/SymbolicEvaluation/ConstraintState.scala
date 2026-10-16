package unaa_lola.SymbolicEvaluation

import unaa_lola.AST._
import unaa_lola.Value._
import unaa_lola.util.{Errors, Logging}
import unaa_lola.SymbolicEvaluation.z3Engine.Z3CombiConstraintState
import unaa_lola.Implementations
import unaa_lola.SymbolicEvaluation.z3Engine.Diagnosis.Z3Diagnosis
import unaa_lola.SymbolicEvaluation.z3Engine.Diagnosis.Z3Diagnosis.{diagToString, diagsToString, containsEmpty}
import unaa_lola.Config


object ConstraintState {

  /**
    * Returns the empty state of an implementation.
    *
    * @param spec The specification
    * @param impl The concrete implementation
    * @param config A configuration
    * @return The empty state
    */
  def getEmptyState(spec: Specification, impl: Implementations.Implementation, config: Config): ConstraintState = {
    if (impl == Implementations.z3) {
      Z3CombiConstraintState.getEmptyState(spec, config)
    } else {
      throw new Errors.InternalException(s"getEmptyState called with non-symbolic implementation $impl")
    }
  }

}

/**
  * Abstract class for a set of constraints corresponding to a given timestamp.
  * It's up to the implementation how the state is stored.
  *
  * @param ts The timestamp this state corresponds to
  */
abstract class ConstraintState(val ts: Int) {

  /**
    * Changes the timestamp.
    *
    * @param ts The new timestamp
    * @return The constraint state with the new timestamp
    */
  def setTs(ts: Int): ConstraintState

  /**
    * Replaces the current timestamp with to and adjusts every timestamp occurring in the state.
    * 
    * @param to The new timestamp 
    * @return The transformed state
    */
  def transformTs(to: Int): ConstraintState

  /**
    * Simplifies the state and returns the resulting state.
    *
    * @param kill The set of symbols that have to be simplified (and thus no longer appear in the new state)
    * @param stage The stage of the simplification strategy to be used
    * @param pred Optionally, the implementation may require the predecessor state. (e.g. to employ widening)
    * @return The simplified state
    */
  def simplify(kill: Set[SymbolIdentifier] = Set(), stage: Int, pred: Option[ConstraintState] = None): ConstraintState

  /**
    * Returns the value of a specified symbol described by this state for a specific timestamp.
    *
    * @param sid The symbol of the desired value
    * @return The value of the symbol 
    */
  def getValue(id: SymbolIdentifier): Value

  /**
    * Shorthand for getValue for a value corresponding to the current time.
    *
    * @param id The symbol of the desired value
    * @return The value of the symbol
    */
  def getCurrentValue(id: Identifier): Value = getValue(SymbolIdentifier(id, ts))
  
  /**
    * Stores the new value of an input stream.
    *
    * @param id The input stream identifier
    * @param input The new value
    * @return The resulting state
    */
  def addInput(id: Identifier, input: Value): ConstraintState

  /**
    * Applies the specification (including assumption) to the state and returns a state that contains constraints describing every stream for the current and the next time step.
    *
    * @param spec The specification (the implementation may require the specification to be flattened)
    * @param maxTs The final timestamp. None if unknown.
    * @return The new constraint state
    */
  def applySpec(spec: Specification, maxTs: Option[Int]): ConstraintState

  /**
    * Applies the specification (including assumption) to the state and returns a state that contains constraints describing every stream for the current and the previous time step.
    *
    * @param spec The specification (the implementation may require the specification to be flattened)
    * @param maxTs The final timestamp. None if unknown.
    * @return The new constraint state
    */
  def applySpecReversed(spec: Specification, maxTs: Option[Int]): ConstraintState

  /**
    * Cleans up the state. 
    * Is usually called instead of [[simplify]] if no simplification is necessary.
    *
    * @param symbolsToKill The set of symbols that should not occurr in the cleaned state
    * @return The cleaned state
    */
  def cleanUp(symbolsToKill: Set[SymbolIdentifier]): ConstraintState

  /**
    * Sets the state for the next time step.
    *
    * @param cs The next step's constraint state
    * @return This states including the next step's state
    */
  def setFuture(cs: ConstraintState): ConstraintState

  /**
    * Removes the future state.
    *
    * @return This state without the next step's state
    */
  def clearFuture(): ConstraintState

  /**
    * Checks whether this state is congruent to another state, i.e. whether the two states represent the same values for all streams.
    *
    * @param other The other state
    * @param ignore Optionally ignore certain symbols. Useful if parts of the concrete state do not get simplified or cleaned up and have therefore symbols that would lead to false outcome here
    * @return True if congruent, false if not
    */
  def isCongruentTo(other: ConstraintState, ignore: Set[SymbolIdentifier] = Set()): Boolean
}

/**
  * Constructs new constraint states based on other constraint states.
  *
  * @param flatspec The specification
  * @param maxTs The maximum timestamp. None if unknown.
  * @param simplification Specifies when, how often and in what way simplification should be applied
  */
class ConstraintStateTransformer(val flatspec: Specification, val simplification: SimplificationStrategy, config: Config) {

  // Sets of stream identifiers (input + non-input) with specific timestamps
  private val identifierMinus1 = flatspec.identifiers.map(SymbolIdentifier(_, -config.keepPastSymbolsFor))
  private val identifierZero = flatspec.identifiers.map(SymbolIdentifier(_, 0))
  private val identifierPlus1 = flatspec.identifiers.map(SymbolIdentifier(_, 1))

  // Sets of all identifiers referenced in stream definitions of the spec timestamped with 0
  private val MOneUsedZero = flatspec.crawlOffsets(-1).map(SymbolIdentifier(_, 0))
  private val POneUsedZero = flatspec.crawlOffsets(1).map(SymbolIdentifier(_, 0))

  private val diagnosis = if (config.diagnosisEnabled) Some(new Z3Diagnosis(flatspec.components.map(Identifier(_, Bool)), config)) else None

  /**
    * Generates a new constraint state by transforming another constraint state into its successor state.
    *
    * @param cs The current contraint state
    * @param inputs A map mapping each input stream identifier to the read value
    * @return The next constraint state
    */
  def computeSuccessor(cs: ConstraintState, inputs: Map[Identifier, Value], futureState: Option[ConstraintState]): ConstraintState = {
    var newConstState = cs

    config.z3RuntimeCollector.reset() // TODO this class should not contain any z3 specific stuff
    val from = System.nanoTime()

    // Possibly simplify the state
    // only keep symbols that will be referenced in the next step, i.e. 0-symbols that occur with -1 offsets in the spec
    //val eraseVars = identifierMinus1// ++ (identifierZero -- MOneUsedZero)
    val actualKeep = if (config.keepPastSymbolsFor > cs.ts) cs.ts else config.keepPastSymbolsFor // TODO maybe disable this as long as keepPastSymbols > cs.ts (none is erased then anyway)
    val eraseVars = flatspec.identifiers.map(SymbolIdentifier(_, -actualKeep - 1))
    if (simplification.optimizationNecessary) {
      newConstState = newConstState.simplify(eraseVars, simplification.optimizationStage)
      Logging.log(s"State after simplification = ${newConstState.toString}", channel=Logging.DEBUG)
    } else {
      newConstState = newConstState.cleanUp(eraseVars)
      Logging.log(s"State after cleanup = ${newConstState.toString}", channel = Logging.DEBUG)
    }
    simplification.proceedStep()

    val to1 = System.nanoTime()

    // Increment timestamp
    newConstState = newConstState.transformTs(newConstState.ts + 1)

    // Set future if future +1 offsets shall be further constrained by a state
    if (futureState.isDefined) {
      newConstState = newConstState.setFuture(futureState.get)
      Logging.log(s"Inserted the following future state: ${futureState.get.toString}", channel=Logging.DEBUG)
    } else {
      newConstState = newConstState.clearFuture()
    }
    // Add inputs
    inputs.foreach {
      case (i, v) => newConstState = newConstState.addInput(i, v)
    }
    Logging.log(s"State after input reading = ${newConstState.toString}", channel=Logging.DEBUG)

    // Apply spec
    newConstState = newConstState.applySpec(flatspec, config.maxTs)
    Logging.log(s"State after specification application = ${newConstState.toString}", channel=Logging.DEBUG)

    val to2 = System.nanoTime()

    // Diagnosis
    diagnosis.foreach(_.doDiagnosis(newConstState.asInstanceOf[Z3CombiConstraintState], config.keepPastSymbolsFor)) // TODO make this implementation agnostic

    val to3 = System.nanoTime()
    Logging.log(s"Simplification took ${(to1 - from) / 1000} μs, Evaluation took ${(to2 - to1) / 1000} μs" + (if (config.collectZ3Runtimes) s", z3 calls took ${config.z3RuntimeCollector.getTotalTimeInMicro} µs" else "" ) + (if (diagnosis.isDefined) s", diagnosis took ${(to3 - to2) / 1000} µs" else "" ))
    Logging.log(s"${(to1 - from) / 1000},${(to2 - to1) / 1000}" + (if (config.collectZ3Runtimes) s",${config.z3RuntimeCollector.getTotalTimeInMicro}" else "")  + (if (diagnosis.isDefined) s",${(to3 - to2) / 1000}" else ""), channel = Logging.RUNTIMES)

    newConstState
  }

  /**
    * Generates a new constraint state by transforming another constraint state into its predecessor state.
    *
    * @param cs The current constraint state
    * @return The previous constraint state
    */
  def computePredecessor(cs: ConstraintState): ConstraintState = {
    // Decrement timestamp
    var newConstState = cs.transformTs(cs.ts - 1)
    Logging.log(s"State after transformation = ${newConstState.toString}", channel=Logging.DEBUG)

    // Apply spec
    newConstState = newConstState.applySpecReversed(flatspec, config.maxTs)
    Logging.log(s"State after reverse specification application = ${newConstState.toString}", channel=Logging.DEBUG)

    // Possibly simplify the state
    // only keep symbols that will be referenced in the previous step, i.e. 0-symbols that occur with +1 offsets in the spec
    val eraseVars = identifierPlus1 ++ (identifierZero -- POneUsedZero)
    if (simplification.optimizationNecessary) {
      newConstState = newConstState.simplify(eraseVars, simplification.optimizationStage, Some(cs))
      Logging.log(s"State after simplification = ${newConstState.toString}", channel=Logging.DEBUG)
    } else {
      newConstState = newConstState.cleanUp(eraseVars)
      Logging.log(s"State after cleanup = ${newConstState.toString}", channel = Logging.DEBUG)
    }
    simplification.proceedStep()

    newConstState
  }

}