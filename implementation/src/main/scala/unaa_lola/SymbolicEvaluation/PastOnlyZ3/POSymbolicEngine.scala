package unaa_lola.SymbolicEvaluation.PastOnlyZ3

import unaa_lola.SymbolicEvaluation.PastOnlyZ3.POValue.Value
import unaa_lola.AST.{Identifier, SupIdentifier, Operator}

/**
 * Representation of a symbolic formula
 * @tparam R Concrete type to which the symbolic formula may be expressed (may be an over-approximation)
 */
abstract class Formula[R] {
  /**
   * Create list of variables the formula is dependent on
   * @return Set of variables the formula depends on
   */
  def dependendVariables: Set[SupIdentifier]

  /**
   * Create value from the formula
   * @return Value representing the formula (may be a concrete value)
   */
  def toValue : Value

  /**
   * Check if formula may be converted to a concrete, certain value
   * @return If formula may be converted to a concrete, certain value
   */
  def isCertain: Boolean

  /**
   * Convert formula to a concrete value representation
   * @return Concrete value representation of the formula (may be an over-approximation if isCertain is false)
   */
  def getValueRepresentation : R

  /** @inheritdoc */
  override def toString: String = super.toString
}

/**
 * Abstract class representing the Optimizer for a Formula
 * @tparam T Type of the formulas that can be optimized with this optimizer
 */
abstract class FormulaOptimization[T<:Formula[_]] {

  /**
   * Simplify a system of formulas
   * @param lfs System of formulas
   * @param spare Set of identifiers to be untouched
   * @param force Force simplification in any way (details left to implementation)
   * @return System of simplified formulas
   */
  def simplify(lfs: Seq[T], spare: Set[SupIdentifier] = Set(), force: Boolean = false): Seq[T]
}

/**
 * Abstract representation of a factory for symbolic formulas as used by the evaluator
 */
abstract class ValueFactory {

  /**
   * Calculates the value of an operation on formulas and constant values
   * @param op The operator
   * @param vals formulas and constant values on which the operation is performed
   * @return Value resulting from the operation (either certain or uncertain)
   */
  def buildFromASTOp(op: Operator, vals: Seq[Value]): Value

  /**
   * Create uncertain constant with a given identifier
   * @param id Identifier to be used
   * @return Constant encoding the identifier
   */
  def buildConstant(id: Identifier): Value
}

/**
 * Abstract representation of an environment capable of performing queries on formulas
 *
 * Only methods for interacting with the evaluator are defined.
 * Details about queries are left open to the implemenations.
 */
abstract class QueryEnvironment {

  /**
   * Add assumption to query environment
   * @param v Value encoding assumption (must be of type boolean)
   */
  def addAssumption(v: Option[Value]): Unit

  /**
   * Clear all assumptions that were previously set
   */
  def clearAssumptions(): Unit

  /**
   * Add assumption encoding the relation given by an if term of form id = if cond then thenExp else elseExp
   * @param id Identifiert the if is assigned to
   * @param cond Consition of the if
   * @param thenExp Then part of the if
   * @param elseExp Else part of the if
   */
  def addIftoAssumption(id: Identifier, cond: Value, thenExp: Value, elseExp: Value): Unit

  /**
   * Add possible value range (inclusive) for a real identifier
   * @param id  Identifier for which a range is known
   * @param from Lower bound (None if not known)
   * @param to Upper bound (None if not known)
   */
  def addRange(id: Identifier, from: Option[Double], to: Option[Double]): Unit

  /**
   * Clear all previously set ranges
   */
  def clearRanges(): Unit

  /**
   * Clear all previously set ranges except for ids in activeIDs
   * @param activeIDs Ids for which ranges shall be preserved
   */
  def clearRangesExceptFrom(activeIDs: Set[Identifier]): Unit


  /**
   * Reset all caches.
   * Must be called if old results of Bound/Satisfiability queries may not hold anymore
   */
  def resetCaches(): Unit
}