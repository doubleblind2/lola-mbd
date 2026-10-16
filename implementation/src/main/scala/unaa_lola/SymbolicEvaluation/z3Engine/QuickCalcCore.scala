package unaa_lola.SymbolicEvaluation.z3Engine

import unaa_lola.AST
import unaa_lola.AST.{Expression, Identifier, Specification}
import unaa_lola.CertainEvaluation.CertainEvaluator
import unaa_lola.Value.Value


/**
  * A QuickCalcCore evaluates a specification under certainty. If a new value for a stream is not fully known, a QCC does nothing.
  * QCCs are used during symbolic evaluation to compute easily computed values much faster than via symbolic reasoning.
  *
  * @param spec The flattened specification
  */
class QuickCalcCore(spec: Specification) {

  private var state : Map[Identifier, Option[Value]] = Map()
  private var lastState : Map[Identifier, Option[Value]] = Map()

  /**
    * Advances this QCC to the next timestamp.
    */
  def nextTs(): Unit = {
    lastState = state;
    state = Map()
  }

  /**
    * Inputs a new value for a stream.
    *
    * @param id The stream identifier
    * @param value The new value
    */
  def input(id: Identifier, value: Value): Unit = {
    state += (id -> Some(value))
  }

  /**
    * Applies the spec to this QCC to generate the new state.
    *
    * @param ts The timestamp of the new state
    * @param maxTs The maximum timestamp
    */
  def applySpec(ts: Int, maxTs: Option[Int]): Unit = {
    def eval(e: Expression): Option[Value] = e match {
      case AST.Application(op, exps) =>
        val args = exps.map(eval)
        if (args.forall(_.isDefined)){
          Some(CertainEvaluator.applyOp(op, args.map(_.get)))
        } else {
          None
        }
      case AST.Offset(_, -1, default) if ts == 0 => Some(default)
      case AST.Offset(id, -1, _) => lastState.getOrElse(id, None)
      case AST.Offset(id, 0, _) => getID(id)
      case AST.Offset(_, 1, default) if ts == maxTs.getOrElse(-1) => Some(default)
      case AST.Constant(v) => Some(v)
      case id: Identifier => getID(id)
      case _ => None
    }

    def getID(id: Identifier): Option[Value] = {
      if (!state.contains(id)) {
        if (spec.definitions.contains(id)) {
          state += (id -> eval(spec.definitions(id)))
        } else {
          state += (id -> None)
        }
      }
      state(id)
    }

    spec.definitions.foreach{
      case(id,_) => getID(id)
    }
  }

  /**
    * Returns the value of a stream at the current timestamp.
    *
    * @param id The stream identifier
    * @return The value if id, None if unavailable
    */
  def get(id: Identifier): Option[Value] = {
    state.getOrElse(id, None)
  }

  /**
    * Returns the value of a stream at the previous timestamp.
    *
    * @param id The stream identifier
    * @return The value if id, None if unavailable
    */
  def getLast(id: Identifier): Option[Value] = {
    lastState.getOrElse(id, None)
  }


}
