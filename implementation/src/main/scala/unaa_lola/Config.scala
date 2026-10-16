package unaa_lola

import unaa_lola.util.Logging
import unaa_lola.Implementations.Implementation
import unaa_lola.SymbolicEvaluation.{SimplificationStrategy, CommonStraegies}
import unaa_lola.AST.{Identifier, SymbolIdentifier}
import com.microsoft.z3.Context
import unaa_lola.util.RuntimeCollector
import java.io.InputStream
import java.io.PrintStream

object CLIConfig {
  var implementation: Implementations.Value = Implementations.certain
}

case class Config(
  // general
  loggingChannels: Set[Logging.CHANNEL] = Set(Logging.TECHNICAL_LOG),
  logFile: Boolean = false,
  noLogging: Boolean = false,
  logRuntimes: Boolean = false,
  specSource: String = "", // CLI only
  inStream: Option[InputStream] = None, // set this to receive inputs from a stream, always on when used as CLI
  outStream: Option[PrintStream] = None, // set this to print outputs on a stream, always on when used as CLI
  csvIn: Boolean = false,
  csvOut: Boolean = false,
  verbose: Boolean = false, // TODO remove this
  maxTs: Option[Int] = None, // set via [[setNumberOfInputs]]
  maxSteps: Option[Int] = None, // abort future estimation after this many steps, all transformers after that will be maximally abstracted
  simplificationStrategy: SimplificationStrategy = CommonStraegies.NoSimplificationStrategy, // set via [[setOptimizationInterval]]
  futureSimplificationStrategy: SimplificationStrategy = CommonStraegies.NoSimplificationStrategy, // set via [[forceWideningAfter]]
  noFuture: Boolean = false, // set this to prevent future estimation, will maximally abstract when used with positive offsets

  // z3 specific
  ctx: Context = {
    val settings = new java.util.HashMap[String, String]()
    settings.put("timeout", "1000")
    new Context(settings)
  },
  noZ3BoundQuery: Boolean = false, // can lead to quicker but less accurate z3 calls
  collectZ3Runtimes: Boolean = false, // also collect share of the runtime attributed to z3 calls, logRuntimes must also be set then
  z3RuntimeCollector: RuntimeCollector = new RuntimeCollector, // TODO maybe consolidate this with above flag, kind of redundant
  keepPastSymbolsFor: Int = 1, // for this many timestamps symbols will be kept, i.e., not pruned

  // diagnosis
  singleDiag: Boolean = false,
  multiDiag: Boolean = false,
  temporalDiag: Boolean = false,
  specificAtempDiag: Option[Set[Identifier]] = None,
  specificTempDiag: Option[Set[SymbolIdentifier]] = None
) {

  val diagnosisEnabled = singleDiag || multiDiag || temporalDiag

  /** Changes the pruing frequency. */
  def setOptimizationInterval(i: Int): Config = {
    require(i >= 0)
    if (i == 0) copy(simplificationStrategy = CommonStraegies.NoSimplificationStrategy)
    else copy(simplificationStrategy = CommonStraegies.IntervalSimplificationStrategy(i))
  }

  /** Enforeces widening if future estimation takes longer than i time steps. */
  def forceWideningAfter(i: Int): Config = {
    require(i >= 0)
    if (i == 0) copy(simplificationStrategy = CommonStraegies.NoSimplificationStrategy)
    else copy(futureSimplificationStrategy = CommonStraegies.WideningOnlySimplificationStrategy(i))
  }

  /** Sets the length of the trace. If not set, if will be Int.MAX_VALUE. Must be set when future offsets are involved. */
  def setNumberOfInputs(numInputs: Int): Config = {
    require(numInputs >= 1)
    copy(maxTs = Some(numInputs - 1))
  }

}

