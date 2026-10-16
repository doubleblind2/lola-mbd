package unaa_lola.RangeEvaluation

import unaa_lola.AST._
import unaa_lola.CertainEvaluation.CertainEvaluator
import unaa_lola.util.Errors.InternalException
import unaa_lola.util.Logging
import unaa_lola.{AST, Evaluator}
import unaa_lola.Value._
import unaa_lola.Config

/**
 * Object with constant functions for evaluating certain LOLA expressions
 */
object ValueRangeEvaluator {

  def toDoubleRange(v: Value): (Double, Double) = {
    v match {
      case RealValue(value) => (value, value)
      case RangeValue(Some(from), Some(to), _) => (from, to)
      case _ => throw new InternalException(s"Range evaluator cannot handle value $v")
    }
  }

  def toBoolRange(v: Value): (Boolean, Boolean) = {
    v match {
      case BoolValue(true) => (true, false)
      case BoolValue(false) => (false, true)
      case _: UnknownBoolValue => (true, true)
    }
  }

  def fromBoolRange(b1: Boolean, b2: Boolean): Value = {
    if (b1 && b2) {
      UnknownBoolValue()
    } else {
      BoolValue(b1)
    }
  }

  def fromDoubleRange(d1: Double, d2: Double): Value = {
    val left = if (d1.isNegInfinity) None else Some(d1)
    val right = if (d2.isPosInfinity) None else Some(d2)
    RangeValue(left, right)
  }

  /**
   * Determines the result of an operator application on a bunch of certain values
   * Fails if uncertain values or values of wrong type are passed.
   *
   * @param op The operator to be applied
   * @param vals The certain values on which the operator is applied
   * @return The result of the application
   */
  def applyOp(op: Operator, vals: Seq[Value]): Value = {

    val nvals = vals.map {
      case RangeValue(Some(f), Some(t), _) if f == t => RealValue(f)
      case v => v
    }

    val allCert = nvals.forall(_.isCertain)

    if (allCert) {
      CertainEvaluator.applyOp(op, nvals)
    } else {
      op match {
        case AST.iff => applyOp(AST.and, Seq(applyOp(AST.imp, nvals), applyOp(AST.imp, nvals.reverse)))
        case AST.imp => applyOp(AST.or, Seq(applyOp(AST.not, Seq(nvals(0))), nvals(1)))
        case AST.gt => applyOp(AST.lt, nvals.reverse)
        case AST.ge => applyOp(AST.le, nvals.reverse)
        case AST.ne => applyOp(AST.not, Seq(applyOp(AST.eq, nvals)))

        case AST.and |
             AST.or |
             AST.not => applyOpBool(op, nvals)

        case AST.plus |
             AST.times |
             AST.lt |
             AST.le => applyOpReal(op, nvals)

        case AST.eq =>
          vals.head.getType match {
            case Real =>
              val ((min1, max1), (min2, max2)) = (toDoubleRange(nvals(0)), toDoubleRange(nvals(1)))
              fromBoolRange((min2 <= min1 && min1 <= max2) || (min1 <= min2 && min2 <= max1), !(min1 == max1 && max1 == min2 && min2 == max2))

            case Bool => applyOpBool(AST.iff, nvals)
          }

        case AST.ite => {
          nvals.head match {
            case BoolValue(value) => if (value) nvals(1) else nvals(2)
            case UnknownBoolValue(_) => nvals(1).getType match {
              case Real =>
                val ((min1, max1), (min2, max2)) = (toDoubleRange(nvals(1)), toDoubleRange(nvals(2)))
                RangeValue(Some(Seq(min1, min2).min), Some(Seq(max1, max2).max))
              case Bool =>
                val ((t1, f1), (t2, f2)) = (toBoolRange(nvals(1)), toBoolRange(nvals(2)))
                fromBoolRange(t1 || t2, f1 || f2)
            }
          }
        }
      }
    }
  }

  private def applyOpReal(op: Operator, vals: Seq[Value]): Value = {

    val ((min1, max1), (min2, max2)) = (toDoubleRange(vals(0)), toDoubleRange(vals(1)))

      op match {
        case AST.plus => RangeValue(Some(min1 + min2), Some(max1 + max2))
        case AST.times => {
          val s1 = Seq(min1 * min2, min1 * max2, min2 * max1, max1 * max2)
          RangeValue(Some(s1.min), Some(s1.max))
        }

        case AST.lt => fromBoolRange(min1 < max2, min2 <= max1)
        case AST.le => fromBoolRange(min1 <= max2, min2 < max1)
      }
  }

  private def applyOpBool(op: Operator, vals: Seq[Value]): Value = {

    val ((t1, f1), (t2, f2)) = (toBoolRange(vals(0)), toBoolRange(vals.lift(1).getOrElse(BoolValue(false))))

      val (rt, rf) = op match {
        case AST.and => (t1 && t2, f1 || f2)
        case AST.or => (t1 || t2, f1 && f2)
        case AST.not => (f1, t1)
      }
      fromBoolRange(rt, rf)
    }
}


/**
 * Class for the evaluation of uncertain LOLA expressions with range abstraction
 * @param spec The specification to be evaluated
 * @param config A configuration
 */
class ValueRangeEvaluator(spec: ExactSpecification, config: Config) extends Evaluator(spec, config, false) {

  var lastState: Map[Identifier, Value] = Map()
  var state: Map[Identifier, Value] = Map()

  /** @inheritdoc */
  def eval(): Unit = {
    lastState = state
    state = Map()

    consumeInputs.foreach{case (stream, value) => readInputVal(stream, value)}

    val from = System.nanoTime();
    flatspec.outputs.foreach {
      o => 
        val value = evalStream(o).get match {
        case RangeValue(from, to, _) => ValueRangeEvaluator.fromDoubleRange(from.get, to.get)
        case v => v
      }
      singleValOutput(o, value)
    }
    val to = System.nanoTime();

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
      case Application(op, exps) => ValueRangeEvaluator.applyOp(op, exps.map(evalExpr(_, trunk)))
      case Offset(id, offset, default) if offset == -1 =>
        evalStream(id, trunk)
        lastState.getOrElse(id, default)
      case Offset(exp, offset, _) if offset == 0 => evalExpr(exp, trunk)
      case _: Offset => throw new InternalException(s"Positive offsets not supported by range evaluation!")
      case Constant(v) => v
      case i: Identifier => evalStream(i, trunk).get
    }
  }

  private def readInputVal(stream: Identifier, value: Value) : Unit = {
    val fixedValue = value match {
      case v: RangeValue => v.finiteRepresentation
      case v: RealSetValue => v.toRangeValue
      case _ => value
    }
    state += (stream -> fixedValue)
  }

}
