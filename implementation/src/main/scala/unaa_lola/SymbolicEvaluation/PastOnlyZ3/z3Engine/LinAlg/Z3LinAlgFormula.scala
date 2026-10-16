package unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.LinAlg

import unaa_lola.AST.{Identifier, SupIdentifier}
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.POValue.{Value, RealVal, UncertainRealValue}
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.Formula
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.Z3QueryEnvironment
import unaa_lola.util.Errors

/**
 * Common constant functions dealing with linear algebra formulas for usage with z3 optimization
 */
object Z3LinAlgFormula {
  def getEnv(s: Seq[Z3LinAlgFormula]): Z3QueryEnvironment = {
    if (s.isEmpty) {
      throw new Errors.InternalException("Z3LinAlgFormula.getEnv called with empty sequence")
    } else if (s.forall(_.env == s.head.env)) {
      s.head.env
    } else {
      throw new Errors.InternalException("Operation applied to formulas with different environment")
    }
  }

  def getEnv(f1: Z3LinAlgFormula, f2: Z3LinAlgFormula): Z3QueryEnvironment = {
    getEnv(Seq(f1, f2))
  }
}

/**
 * Class representing a linear algebra formula for usage with z3 optimization:
 * c1*v1 + c2*v2 + ... + cn*vn + c0
 * where ci are Real constants and vi are variables of type real
 * @param coeff Map of the coefficients (except c0) in the form Map(v1 -> c1, v2 -> c2, ..., vn -> cn)
 * @param offset The offset c0
 * @param env The corresponding z3 environment
 */
case class Z3LinAlgFormula(coeff: Map[Identifier, Double], offset: Double, env: Z3QueryEnvironment) extends Formula[(Option[Double], Option[Double])] {

  /**
   * Calculates maximal and minimal value of the formula
   * @param boundingMap Map with boundings for all variables
   * @return Bounding for the linear algebra formula
   */
  override def getValueRepresentation: (Option[Double], Option[Double]) = env.boundQuery(this)

  /** @inheritdoc */
  override def isCertain: Boolean = {
    val bounds = getValueRepresentation
    bounds._1.isDefined && bounds._1 == bounds._2
  }

  /** @inheritdoc */
  override def toValue : Value = {
    val bounds = getValueRepresentation
      if (isCertain) {
      RealVal(bounds._1.get)
    } else {
      UncertainRealValue(this)
    }
  }

  /** @inheritdoc */
  override def dependendVariables: Set[SupIdentifier] = coeff.keys.toSet

  /** @inheritdoc */
  override def toString: String = {
      coeff.map{case (k,v) => s"(${String.format("%.2f", v)} * $k)"}.mkString(" + ") + s" + ${String.format("%.2f", offset)}"
  }
}
