package unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.LinAlg

import unaa_lola.AST.{Identifier, Operator}
import unaa_lola.AST
import unaa_lola.Value.Real
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.POValue.{UncertainBoolValue, Value, RealVal, UncertainRealValue}
import unaa_lola.SymbolicEvaluation.z3Engine.LinAlgCore.newVarId
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.Z3QueryEnvironment
import unaa_lola.util.{Errors, Logging}


/**
 * Class containing standard functions to create [[Z3LinAlgFormula]]
 * @param env The corresponding z3 environment
 */
class Z3LinAlgFormulaFactory(env: Z3QueryEnvironment) {
  /**
   * Creates the linear algebra formula 1*id
   * @param id The identifier of the formula
   * @return The linear algebra formula
   */
  def const(id: Identifier): Z3LinAlgFormula = {
    new Z3LinAlgFormula(Map() + (id -> 1), 0, env)
  }

  /**
   * Creates the linear algebra formula n
   * @param n The offset of the formula
   * @return The linear algebra formula
   */
  def numConst(n: Double): Z3LinAlgFormula = {
    new Z3LinAlgFormula(Map(), n, env)
  }

  /**
   * Adds two linear algebra formulas
   * @param f1 Formula 1
   * @param f2 Formula 2
   * @return The linear algebra formula resulting from adding f1 and f2
   */
  def plus(f1: Z3LinAlgFormula, f2: Z3LinAlgFormula): Z3LinAlgFormula = {

    val newCoeff = f1.coeff.foldLeft[Map[Identifier, Double]](f2.coeff){
      case (curr, (id, n)) =>
        val v = n + curr.getOrElse(id, 0.0)
        if (v == 0) curr.removed(id) else curr + (id -> v)
    }

    new Z3LinAlgFormula(newCoeff, f1.offset + f2.offset, env)
  }

  /**
   * Multiplies two linear algebra formulas
   *
   * Take care: This operation results in a non-linear algebra function, if not one of the two formulas is constant.
   * If this is the case a new variable is introduced which has the boundings of the used formulas, however
   * interconnections may get lost this way.
   * If this is the case a corresponding warning is printed during execution on stderr.
   *
   * @param f1 Formula 1
   * @param f2 Formula 2
   * @return The linear algebra formula resulting from multiplying f1 and f2
   */
  def times(f1: Z3LinAlgFormula, f2: Z3LinAlgFormula): Z3LinAlgFormula = {
    if (f1.isCertain || f2.isCertain) {
      val const = if (f1.isCertain) f1.getValueRepresentation._1.get else f2.getValueRepresentation._1.get
      val nconst = if (f1.isCertain) f2 else f1

      new Z3LinAlgFormula(nconst.coeff.map{case (k,v) => (k, v * const)}, const * nconst.offset, env)
    } else {
      Logging.log("Multiplication of two variables occured. Approximation performed", Logging.WARNING)

      val int1 = f1.getValueRepresentation
      val int2 = f2.getValueRepresentation

      def isNegativeLB(i: (Option[Double], Option[Double])) = i._1.getOrElse(-1.0) < 0
      def isNegativeUB(i: (Option[Double], Option[Double])) = i._2.getOrElse(1.0) < 0

      val p1 = int1._1.flatMap(a => int2._1.flatMap(b => Some(a * b)))
      val p2 = int1._1.flatMap(a => int2._2.flatMap(b => Some(a * b)))
      val p3 = int1._2.flatMap(a => int2._1.flatMap(b => Some(a * b)))
      val p4 = int1._2.flatMap(a => int2._2.flatMap(b => Some(a * b)))

      val p1neg = (isNegativeLB(int1) != isNegativeLB(int2))
      val p2neg = (isNegativeLB(int1) != isNegativeUB(int2))
      val p3neg = (isNegativeUB(int1) != isNegativeLB(int2))
      val p4neg = (isNegativeUB(int1) != isNegativeUB(int2))

      val min = Seq((p1, p1neg), (p2, p2neg), (p3, p3neg), (p4, p4neg)).filter{case (e, p) => e.isDefined || !p}.sortWith{case (e1, e2) => e1._1.isEmpty || (e2._1.isDefined && e1._1.get < e2._1.get)}.head._1
      val max = Seq((p1, p1neg), (p2, p2neg), (p3, p3neg), (p4, p4neg)).filter{case (e, p) => e.isDefined || p}.sortWith{case (e1, e2) => e1._1.isEmpty || (e2._1.isDefined && e1._1.get > e2._1.get)}.head._1

      val ni = Identifier(s"m#$newVarId", Real)
      env.addRange(ni, min, max)
      new Z3LinAlgFormula(Map(ni -> 1), 0, env)
    }
  }

  /**
   * Calculates the value of an operation on linear algebra formulas and constant values
   * @param op Operator on linear algebra formulas: ==, !=, <, >, <=, >=, +, *
   * @param vals Linear algebra formulas and constant values on which the operation is performed
   * @param laq LinearAlgQuery object in case Z3 related stuff is required
   * @return Value resulting from the operation (either certain or uncertain)
   */
  def buildFromASTOp(op: Operator, vals: Seq[Value]): Value = {

    val nv = vals.map {
      case RealVal(value) =>
        numConst(value)
      case UncertainRealValue(value: Z3LinAlgFormula) =>
        value
      case v =>
        throw new Errors.InternalException(s"LinAlgFormula.solve called with non-bool argument $v")
    }

    op match {
      case AST.eq | AST.ne | AST.lt | AST.gt | AST.le | AST.ge =>
        UncertainBoolValue(env.getLogicalFormulaFactory.linAlgQueryConst(op, vals))
      case AST.plus =>
        plus(nv(0), nv(1)).toValue
      case AST.times =>
        times(nv(0), nv(1)).toValue
      case _ =>
        throw new Errors.InternalException(s"LinAlgFormula.solve called with unknown operator $op")
    }
  }
}
