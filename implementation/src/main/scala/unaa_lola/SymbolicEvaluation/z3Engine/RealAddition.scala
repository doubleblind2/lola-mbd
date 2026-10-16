package unaa_lola.SymbolicEvaluation.z3Engine

import com.microsoft.z3.{Context, Expr, RealSort}
import unaa_lola.AST.SupIdentifier
import unaa_lola.Value.RealValue

import unaa_lola.AST
import unaa_lola.util.Errors

/**
  * Object containing common functions to build RealAdditions
  */
object RealAddition {

  /**
    * Builds a RealAddition vom a real.
    *
    * @param v The real value as double
    * @return A RealAddition only consisting of an offset v 
    */
  def realAdditionFromValue(v: Double): RealAddition = {
    RealAddition(Map(), v)
  }

  /**
    * Builds a RealAddition from an identifier.
    *
    * @param id The identifier
    * @return A RealAddition of the form 1*id + 0
    */
  def realAdditionFromId(id: SupIdentifier): RealAddition = {
    RealAddition(Map(id -> 1), 0)
  }

  /**
    * Builds a RealAddition by combining multiple other RealAdditions with an operator (+ or *).
    * This is the same as repeatedly call this function's variant with only two RealAddition.
    *
    * @param op The operator
    * @param ras The sequence of sub RealAdditions
    * @return The newly combined RealAddition
    */
  def buildRealAdditionFromApp(op: AST.Operator, ras: Seq[RealAddition]): RealAddition = {
    ras.reduce[RealAddition]{case (ra1, ra2) => buildRealAdditionFromApp(op, ra1, ra2)}
  }

  /**
    * Builds a RealAddition by combining two other RealAdditions with an operator (+ or *).
    * For + this adds coefficients of the same identifier, e.g. (2x+3y+4) + (5y+6z+7) = 2x+9y+6z+11.
    * For * it is assumed that at least one of ra1 or r2 is a constant, i.e. only an offset.
    * Every coefficient is then simply multiplied by this constant, e.g. (2x+3y+4) * 2 = 4x+6y+8.
    *
    * @param op The operator
    * @param ra1 The first sub RealAddition
    * @param ra2 The second sub RealAddition
    * @return The newly combined RealAddition
    */
  def buildRealAdditionFromApp(op: AST.Operator, ra1: RealAddition, ra2: RealAddition): RealAddition = {
    val RealAddition(c1, o1) = ra1
    val RealAddition(c2, o2) = ra2

    op match {
      case AST.plus =>
        val newCoeff = c1.foldLeft[Map[SupIdentifier, Double]](c2) {
          case (curr, (id, n)) =>
            val v = n + curr.getOrElse(id, 0.0)
            if (v == 0) curr.removed(id) else curr + (id -> v)
        }
        RealAddition(newCoeff, o1 + o2)

      case AST.times =>
        if (c1.isEmpty || c2.isEmpty) {
          val const = (if (c1.isEmpty) o1 else o2)
          val RealAddition(coeffs, offset) = if (c1.isEmpty) ra2 else ra1
          RealAddition(coeffs.map{case (k,v) => (k, v * const)}, const * offset)
        } else {
          //TODO support multiplication of two non-constant expressions (probably requires major refactoring)
          throw new Errors.UnAALOLAException(s"Multiplication of two non-constants occurred: ($ra1) * ($ra2)", panic = true)
        }

      case _ => throw new Errors.InternalException(s"Real addition requested for unsupported operation: $op", true)
    }
  }

}

/**
  * Container for linear algebraic formulas of the form:
  * c1*x1 + c2*x2 + ... + cn*xn + c0
  *
  * @param coeff Maps variables xi to coefficients ci
  * @param offset The component c0
  */
case class RealAddition(coeff: Map[SupIdentifier, Double], offset: Double) {
  
  /** @inheritdoc */
  override def toString: String = {
    if (coeff.isEmpty) {
      offset.toString
    } else {
      coeff.map{case (id, c) => s"$c * $id"}.mkString(" + ") + (if (offset != 0) s" + $offset" else "")
    }
  }

  /**
    * Returns the set of variables.
    *
    * @return The set of variables
    */
  def getIds: Set[SupIdentifier] = coeff.keySet

  /**
    * Checks whether this real addition is a constant value, i.e. if all variables are only multiplied by 0
    *
    * @return True if constant
    */
  def isConst: Boolean = coeff.forall{case (_,c) => c == 0}

  /**
    * Replaces variables with other variables.
    * The mapping which variables shall be replaced by which other variable is given as two sequences, 
    * s.t. the i-th element from f is replaces by the i-th tlemenet from t.
    *
    * @param f The variables to be replaced
    * @param t The variables to be replaced with
    * @return The new real addition
    */
  def replaceIDs(f: Seq[SupIdentifier], t: Seq[SupIdentifier]): RealAddition = {
    copy(coeff = coeff.map {
      case (i, c) => 
        if (f.contains(i)) {
        (t(f.indexOf(i)), c)
        } else {
        (i, c)
        }
    })
  }

  /**
    * Produces a Z3 expression representing this RealAddition.
    *
    * @param ctx The Z3 context
    * @return The Z3 expression representing this RealAddition
    */
  def toZ3Exp(ctx: Context): Expr[RealSort] = {
    coeff.foldLeft[Expr[RealSort]](ctx.mkReal(offset.toString)) {
      case (t, (_, 0)) => t
      case (t, (i, c)) => ctx.mkAdd(t, ctx.mkMul(ctx.mkReal(c.toString), ctx.mkRealConst(i.toString)))
    }
  }
}