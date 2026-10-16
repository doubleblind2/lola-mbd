package unaa_lola.SymbolicEvaluation

import unaa_lola.AST._
import unaa_lola.Value._
import unaa_lola.util.Errors.InternalException
import unaa_lola.util.{Errors, Logging}
import unaa_lola.{AST, Evaluator}
import unaa_lola.Implementations
import unaa_lola.SymbolicEvaluation.z3Engine.Diagnosis.Z3Diagnosis
import unaa_lola.SymbolicEvaluation.z3Engine.Z3CombiConstraintState
import unaa_lola.Config


/**
 * Class for the evaluation of uncertain LOLA expressions with symbolic calculation
 * 
 * @param impl The implementation for constraint states (and hence also the SMT solver) to be used
 * @param spec The specification to be evaluated
 * @param config A configuration
 */
class SymbolicEvaluator(impl: Implementations.Implementation, spec: ExactSpecification, config: Config) 
  extends Evaluator(spec, config, unaa_lola.Implementations.supportsEnums(impl)) {

  private var inputs: Map[Identifier, Value] = Map()
  private var future: Option[Future] = None

  val transformer = new ConstraintStateTransformer(flatspec, config.simplificationStrategy, config)
  var cs: ConstraintState = ConstraintState.getEmptyState(flatspec, impl, config)

  if (!config.noFuture) {
    val from = System.nanoTime()
    future = Some(new FutureEstimation(impl, config, flatspec, config.futureSimplificationStrategy).constructFuture(config.maxSteps))
    val to = System.nanoTime()
    
    Logging.log(s"Future estimation consisting of ${future.get.states.length} states has been computed. Took: ${(to - from) / 1000} μs")
    Logging.log(s"FutureEstimation returned future:\n${future.get}", channel = Logging.DEBUG)
  }  

  /** @inheritdoc */
  def eval(): Unit = {
    consumeInputs.foreach{case (stream, value) => readInputVal(stream, value)}
    val futureState = if (future.isEmpty || (config.maxTs.isDefined && ts == config.maxTs.get)) None else future.get.getStateEstimation(ts + 1)
    cs = transformer.computeSuccessor(cs, inputs, futureState)

    flatspec.outputs.foreach {
      id => singleValOutput(id, cs.getCurrentValue(id))
    }
  }

  private def readInputVal(stream: Identifier, value: Value): Unit = inputs += (stream -> value)

}
