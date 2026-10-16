package unaa_lola.CertainEvaluation

import unaa_lola.AST._
import unaa_lola.util.Errors.InternalException
import unaa_lola.util.Logging
import unaa_lola.{AST, Evaluator}
import unaa_lola.Value._
import unaa_lola.Config

/**
 * Object with constant function for evaluating certain LOLA expressions
 */
object CertainEvaluator {

  /**
   * Determines the result of an operator application on a bunch of certain values
   * Fails if uncertain values or values of wrong type are passed.
   *
   * @param op The operator to be applied
   * @param vals The certain values on which the operator is applied
   * @return The result of the application
   */
  def applyOp(op: Operator, vals: Seq[Value]): Value = {
    op match {
      case AST.eq => BoolValue(vals(0) == vals(1))
      case AST.ne => BoolValue(vals(0) != vals(1))
      case AST.and => BoolValue(vals(0).asInstanceOf[BoolValue].v && vals(1).asInstanceOf[BoolValue].v)
      case AST.or => BoolValue(vals(0).asInstanceOf[BoolValue].v || vals(1).asInstanceOf[BoolValue].v)
      case AST.not => BoolValue(!vals(0).asInstanceOf[BoolValue].v)
      case AST.iff => BoolValue(!(vals(0).asInstanceOf[BoolValue].v ^ vals(1).asInstanceOf[BoolValue].v))
      case AST.imp => BoolValue(!vals(0).asInstanceOf[BoolValue].v || vals(1).asInstanceOf[BoolValue].v)
      case AST.plus => RealValue(vals(0).asInstanceOf[RealValue].v + vals(1).asInstanceOf[RealValue].v)
      case AST.times => RealValue(vals(0).asInstanceOf[RealValue].v * vals(1).asInstanceOf[RealValue].v)
      case AST.lt => BoolValue(vals(0).asInstanceOf[RealValue].v < vals(1).asInstanceOf[RealValue].v)
      case AST.gt => BoolValue(vals(0).asInstanceOf[RealValue].v > vals(1).asInstanceOf[RealValue].v)
      case AST.le => BoolValue(vals(0).asInstanceOf[RealValue].v <= vals(1).asInstanceOf[RealValue].v)
      case AST.ge => BoolValue(vals(0).asInstanceOf[RealValue].v >= vals(1).asInstanceOf[RealValue].v)
      case AST.ite => if (vals(0).asInstanceOf[BoolValue].v) vals(1) else vals(2)
    }
  }
}

/**
 * Class for the evaluation of certain LOLA expressions
 * @param spec The specification to be evaluated
 * @param config A configuration
 */
class CertainEvaluator(spec: ExactSpecification, config: Config) extends Evaluator(spec, config, true) {

  var lastState: Map[Identifier, Value] = Map()
  var state: Map[Identifier, Value] = Map()

  /** @inheritdoc */
  def eval(): Unit = {
    lastState = state
    state = Map()

    consumeInputs.foreach{case (stream, value) => readInputVal(stream, value)}

    val from = System.nanoTime
    flatspec.outputs.foreach{
      o => singleValOutput(o, evalStream(o).get)
    }
    val to = System.nanoTime

    Logging.log(s"Evaluation took ${(to - from) / 1000} μs")
    Logging.log(s"${(to - from) / 1000}", channel = Logging.RUNTIMES)
  }

  private def evalStream(id: Identifier, trunk: Set[Identifier] = Set()): Option[Value] = {
    if (!state.contains(id) && !trunk.contains(id)) {
      val res = evalExpr(flatspec.definitions(id), trunk + id)
      state += (id -> res)
    }
    state.get(id)
  }

  private def evalExpr(exp: Expression, trunk: Set[Identifier]): Value = {
    exp match {
      case Application(op, exps) => CertainEvaluator.applyOp(op, exps.map(evalExpr(_, trunk)))
      case Offset(id, offset, default) if offset == -1 =>
        evalStream(id, trunk)
        lastState.getOrElse(id, default)
      case Offset(exp, offset, _) if offset == 0 => evalExpr(exp, trunk)
      case _: Offset => throw new InternalException(s"Positive offsets not supported by certain evaluation!")
      case Constant(v) => v
      case i: Identifier => evalStream(i, trunk).get
      case _ => throw new InternalException(s"Expression $exp not supported in Certain evaluation")
    }
  }

  private def readInputVal(stream: Identifier, value: Value): Unit = {
    value match {
      case BoolValue(_) | RealValue(_) | EnumValue(_, _) => state += (stream -> value)
      case _ => throw new InternalError(s"Value $value not supported by CertainEvaluator")
    }
  }

}
