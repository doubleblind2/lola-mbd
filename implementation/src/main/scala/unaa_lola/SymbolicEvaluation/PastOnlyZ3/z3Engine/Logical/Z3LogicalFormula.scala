package unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.Logical

import com.microsoft.z3.BoolExpr
import unaa_lola.AST.{SupIdentifier}
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.POValue.{BoolVal, UncertainBoolValue, Value}
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.Formula
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.Z3QueryEnvironment
import unaa_lola.util.Errors

/**
 * Common constant functions dealing with logical formulas for usage with z3 optimization
 */
object Z3LogicalFormula {
  def getEnv(s: Seq[Z3LogicalFormula]): Z3QueryEnvironment = {
    if (s.isEmpty) {
      throw new Errors.InternalException("Z3LogicalFormula.getEnv called with empty sequence")
    } else if (s.forall(_.env == s.head.env)) {
      s.head.env
    } else {
      throw new Errors.InternalException("Operation applied to formulas with different environment")
    }
  }

  def getEnv(f1: Z3LogicalFormula, f2: Z3LogicalFormula): Z3QueryEnvironment = {
    getEnv(Seq(f1, f2))
  }
}

/**
 * Class representing a logical formula for usage with z3 optimization.
 * @param vars Set of variables referenced in the formula
 * @param z3Shortcut z3 representation of the formula
 * @param env The corresponding z3 environment
 */
class Z3LogicalFormula(val vars: Set[SupIdentifier], val z3Shortcut: BoolExpr, val env: Z3QueryEnvironment) extends Formula[Option[Boolean]] {

  /** @inheritdoc */
  override def getValueRepresentation: Option[Boolean] = env.checkConformance(z3Shortcut)

  /** @inheritdoc */
  override def isCertain: Boolean = {
    val res = getValueRepresentation
    res.isDefined
  }

  /** @inheritdoc */
  override def toValue : Value = {
    val res = getValueRepresentation
    res match {
      case Some(value) => BoolVal(value)
      case None => UncertainBoolValue(this)
    }
  }

  /** @inheritdoc */
  override def dependendVariables: Set[SupIdentifier] = vars

  /** @inheritdoc */
  override def toString: String = s"? //$z3Shortcut".replace("\n", "")
}
