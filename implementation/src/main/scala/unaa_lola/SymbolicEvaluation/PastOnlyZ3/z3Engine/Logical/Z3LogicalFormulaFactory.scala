package unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.Logical

import com.microsoft.z3.BoolExpr
import unaa_lola.AST
import unaa_lola.AST.{Identifier, Operator, SupIdentifier}
import unaa_lola.Value.Bool
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.POValue.{Value, BoolVal, UncertainBoolValue}
import unaa_lola.SymbolicEvaluation.z3Engine.LinAlgCore.newVarId
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.LinAlg.Z3LinAlgFormula
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.Z3QueryEnvironment
import unaa_lola.util.Errors
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.PastOnlyZ3Evaluator.ReplacementIdentifier

/**
 * Class containing standard functions to create [[Z3LogicalFormula]]
 * @param env The corresponding z3 environment
 */
class Z3LogicalFormulaFactory(env: Z3QueryEnvironment) {

  private def checkEnv(f1: Z3LogicalFormula, f2: Z3LogicalFormula): Unit = Z3LogicalFormula.getEnv(f1, f2) == env

  /**
   * Creates a LinAlg formula consisting of a single identifier (may be complex term over reals, i.e. x < 4.55)
   * @param id The identifier. May be replacement identifier to encode complex real term
   * @return Resulting [[Z3LogicalFormula]]
   */
  def const(id: SupIdentifier): Z3LogicalFormula = {
    val z3Formula = id match {
      case Identifier(name, Bool) => env.ctx.mkBoolConst(name)
      case r: ReplacementIdentifier => Z3QueryEnvironment.buildZ3Expression(r, env.ctx).asInstanceOf[BoolExpr]
    }

    new Z3LogicalFormula(Set(id), z3Formula, env)
  }

  /**
   * Creates a LinAlg formula consisting of a term over reals, i.e. x < 4.55)
   * @param op The operator of the term
   * @param vals Parameter values of the term
   * @return Resulting [[Z3LogicalFormula]]
   */
  def linAlgQueryConst(op: Operator, vals: Seq[Value]): Z3LogicalFormula = {
    const(ReplacementIdentifier(s"REPL#$newVarId", Bool, op, vals))
  }

  /**
   * Create true term
   * @return Resulting [[Z3LogicalFormula]]
   */
  def trueVal: Z3LogicalFormula = {
    new Z3LogicalFormula(Set(), env.ctx.mkTrue(), env)
  }

  /**
   * Create false term
   * @return Resulting [[Z3LogicalFormula]]
   */
  def falseVal: Z3LogicalFormula = {
    new Z3LogicalFormula(Set(), env.ctx.mkFalse(), env)
  }

  /**
   * Create conjunction of two logical formulas
   * @param f1 Left-hand formula
   * @param f2 Right-hand formula
   * @return Resulting [[Z3LogicalFormula]]
   */
  def and(f1: Z3LogicalFormula, f2: Z3LogicalFormula): Z3LogicalFormula = {
    checkEnv(f1, f2)
    new Z3LogicalFormula(f1.vars ++ f2.vars, env.ctx.mkAnd(f1.z3Shortcut, f2.z3Shortcut), env)
  }

  /**
   * Create disjunction of two logical formulas
   * @param f1 Left-hand formula
   * @param f2 Right-hand formula
   * @return Resulting [[Z3LogicalFormula]]
   */
  def or(f1: Z3LogicalFormula, f2: Z3LogicalFormula): Z3LogicalFormula = {
    checkEnv(f1, f2)
    new Z3LogicalFormula(f1.vars ++ f2.vars, env.ctx.mkOr(f1.z3Shortcut, f2.z3Shortcut), env)
  }

  /**
   * Create xor of two logical formulas
   * @param f1 Left-hand formula
   * @param f2 Right-hand formula
   * @return Resulting [[Z3LogicalFormula]]
   */
  def xor(f1: Z3LogicalFormula, f2: Z3LogicalFormula): Z3LogicalFormula = {
    checkEnv(f1, f2)
    new Z3LogicalFormula(f1.vars ++ f2.vars, env.ctx.mkXor(f1.z3Shortcut, f2.z3Shortcut), env)
  }

  /**
   * Create equivalence (iff/<->/=) of two logical formulas
   * @param f1 Left-hand formula
   * @param f2 Right-hand formula
   * @return Resulting [[Z3LogicalFormula]]
   */
  def iff(f1: Z3LogicalFormula, f2: Z3LogicalFormula): Z3LogicalFormula = {
    checkEnv(f1, f2)
    new Z3LogicalFormula(f1.vars ++ f2.vars, env.ctx.mkIff(f1.z3Shortcut, f2.z3Shortcut), env)
  }

  /**
   * Create negation of a logical formula
   * @param f Formula to be negated
   * @return Resulting [[Z3LogicalFormula]]
   */
  def not(f: Z3LogicalFormula): Z3LogicalFormula = {
    new Z3LogicalFormula(f.vars, f.env.ctx.mkNot(f.z3Shortcut), f.env)
  }

  /**
   * Calculates the value of an operation on logical formulas and constant values
   * @param op Operator on logical formulas: ==, !=, &&, ||, !, <->, ->, ite
   * @param vals Logical formulas and constant values on which the operation is performed
   * @return Value resulting from the operation (either certain or uncertain)
   */
  def buildFromASTOp(op: Operator, vals: Seq[Value]): Value = {
    val nv = vals.map {
      case BoolVal(value) => if (value) trueVal else falseVal
      case UncertainBoolValue(value: Z3LogicalFormula) => value
      case v => throw new Errors.InternalException(s"Z3LogicalFormula.solve called with non z3 argument $v")
    }

    UncertainBoolValue(op match {
      case AST.ne => xor(nv(0), nv(1))
      case AST.and => and(nv(0), nv(1))
      case AST.or => or(nv(0), nv(1))
      case AST.not => not(nv(0))
      case AST.iff | AST.eq => iff(nv(0), nv(1))
      case AST.imp => or(not(nv(0)), nv(1))
      case AST.ite => or(and(nv(0),nv(1)), and(not(nv(0)),nv(2)))
      case _ => throw new Errors.InternalException(s"LogicalFormula.solve called with unknown operator $op")
    })
  }

}