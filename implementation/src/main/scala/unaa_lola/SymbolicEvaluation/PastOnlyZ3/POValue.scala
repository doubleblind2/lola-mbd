package unaa_lola.SymbolicEvaluation.PastOnlyZ3

import unaa_lola.SymbolicEvaluation.PastOnlyZ3.Formula
import unaa_lola.util.Common.OInterval
import unaa_lola.Value.Type
import unaa_lola.Value.Real
import unaa_lola.Value.Bool

/**
 * Contains classes which are building the AST of the specification and also values which are used for symbolic calculation
 */

object POValue {
  
  //Values

  /**
   * Values used in specification and during evaluation. Also used for values which are terms over symbolic variables
   */
  sealed trait Value {
    /**
     * Indicates if the value is a bool or real constant and not dependent on symbolic values
     */
    def isCertain: Boolean = true

    /**
     * Indicates if the value is dependent on more than one symbolic variable and hence a candidate for simplification
     */
    def simplificationPossible: Boolean = depVarNum > 1

    def materialize: Value = this

    /**
     * Number of symbolic variables the value depends on
     */
    def depVarNum : Int = 0

    def getType: Type
  }

  /**
   * A constant value  of type real
   * @param value The value of the constant
   */
  case class RealVal(value: Double) extends Value {
    override def simplificationPossible: Boolean = false
    override def toString: String = String.format("%.2f", value)

    override def getType: Type = Real
  }

  /**
   * A constant value of type bool
   * @param value The value of the constant
   */
  case class BoolVal(value: Boolean) extends Value {
    override def simplificationPossible: Boolean = false
    override def toString: String = value.toString

    override def getType: Type = Bool
  }

  //Uncertainty

  /**
   * A real value which is uncertain, i.e. contains symbolic identifiers
   * @param value The uncertain value as linear algebra formula
   */
  case class UncertainRealValue(formula: Formula[OInterval]) extends Value {

    override def depVarNum : Int = formula.dependendVariables.size

    override def materialize: Value =  formula.toValue

    override def toString: String = s"[$formula]"

    override def isCertain: Boolean = false

    override def getType: Type = Real
  }

  /**
   * A bool value which is uncertain, i.e. contains symbolic identifiers
   * @param value The uncertain value as logical formula
   */
  case class UncertainBoolValue(formula: Formula[Option[Boolean]]) extends Value {

    override def depVarNum : Int = formula.dependendVariables.size

    override def materialize: Value = formula.toValue

    override def toString: String = s"[$formula]"

    override def isCertain: Boolean = false

    override def getType: Type = Bool
  }


  //Simple Abstraction

  case class RealRange(from: Double, to: Double) extends Value {

    override def depVarNum : Int = 0

    override def materialize: Value = if (from == to) RealVal(from) else this

    override def toString: String = s"[${String.format("%.2f", from)}|${String.format("%.2f", to)}]"
    //override def toString: String = s"?"

    override def isCertain: Boolean = false

    override def getType: Type = Real

  }

  case object UnknownBool extends Value {

    override def depVarNum : Int = 0

    override def materialize: Value = this

    override def toString: String = "?"

    override def isCertain: Boolean = false

    override def getType: Type = Bool

  }

}
