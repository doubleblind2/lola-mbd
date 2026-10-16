package unaa_lola.SymbolicEvaluation.z3Engine

import com.microsoft.z3.{BoolSort, Context, Expr, RealSort, Sort}
import unaa_lola.AST
import unaa_lola.AST.{Expression, Operator, SupIdentifier, SymbolIdentifier}
import unaa_lola.Value._
import unaa_lola.util.Common.OInterval
import unaa_lola.util.Errors
import com.microsoft.z3.Status
import unaa_lola.util.Logging
import unaa_lola.Config

/**
  * Object containing common utility functions for interacting with Z3
  */
object Z3Utilities {

  /**
    * Produces a Z3 variable representing an identifier.
    * The variable's type is the same as the identifier's type.
    * The variable's name is the string representing the identifier.
    *
    * @param id The identifier represented by the variable
    * @param ctx The Z3 context
    * @return The new Z3 variable
    */
  def mkVariableExpression(id: SupIdentifier, ctx: Context): Expr[_] = {
    id.getType match {
      case Bool => ctx.mkBoolConst(id.toString)
      case Real => ctx.mkRealConst(id.toString)
    }
  }

  /**
    * Produces a Z3 variable representing an identifier at a certain timestamp (e.g. for offsets).
    * The variable's type is the same as the identifier's type.
    * The variable's name is the name of the [[SymbolIdentifier]] built from the identifier and timestamp.
    *
    * @param id The identifier represented by the variable
    * @param o The timestamp (offset of the stream)
    * @param ctx The Z3 context
    * @return The new Z3 variable
    */
  def mkIdExpr(id: AST.Identifier, o: Int, ctx: Context): Expr[_] = {
    val nid = SymbolIdentifier(id, o)
    mkVariableExpression(nid, ctx)
  }

  /**
    * Produces a Z3 constant representing a [[Value]].
    *
    * @param v The value represented by the constant. Must be a known value.
    * @param ctx The Z3 context
    * @return The new Z3 constant
    */
  def mkValueExpr(v: Value, ctx: Context): Expr[_] = {
    v match {
      case RealValue(value) => ctx.mkReal(value.toString)
      case BoolValue(value) => ctx.mkBool(value)
      case _ => throw new Errors.InternalException(s"mkValueExpr called for non-constant value: $v", true)
    }
  }

  /**
    * Produces a Z3 expression representing the application of an [[Operator]] on a sequence of Z3 expressions.
    *
    * @param op The operator of the application
    * @param exps The sequence of Z3 expressions
    * @param ctx The Z3 context
    * @return The new Z3 expression
    */
  def mkZ3ExpressionfromApp(op: Operator, exps: Seq[Expr[_]], ctx: Context): Expr[_] = {
    op match {
      case AST.eq => ctx.mkEq(exps(0), exps(1))
      case AST.ne => ctx.mkNot(ctx.mkEq(exps(0), exps(1)))
      case AST.and => ctx.mkAnd(exps(0).asInstanceOf[Expr[BoolSort]], exps(1).asInstanceOf[Expr[BoolSort]])
      case AST.or => ctx.mkOr(exps(0).asInstanceOf[Expr[BoolSort]], exps(1).asInstanceOf[Expr[BoolSort]])
      case AST.not => ctx.mkNot(exps(0).asInstanceOf[Expr[BoolSort]])
      case AST.iff => ctx.mkIff(exps(0).asInstanceOf[Expr[BoolSort]], exps(1).asInstanceOf[Expr[BoolSort]])
      case AST.imp => ctx.mkImplies(exps(0).asInstanceOf[Expr[BoolSort]], exps(1).asInstanceOf[Expr[BoolSort]])
      case AST.plus => ctx.mkAdd(exps(0).asInstanceOf[Expr[RealSort]], exps(1).asInstanceOf[Expr[RealSort]])
      case AST.times => ctx.mkMul(exps(0).asInstanceOf[Expr[RealSort]], exps(1).asInstanceOf[Expr[RealSort]])
      case AST.lt => ctx.mkLt(exps(0).asInstanceOf[Expr[RealSort]], exps(1).asInstanceOf[Expr[RealSort]])
      case AST.gt => ctx.mkGt(exps(0).asInstanceOf[Expr[RealSort]], exps(1).asInstanceOf[Expr[RealSort]])
      case AST.le => ctx.mkLe(exps(0).asInstanceOf[Expr[RealSort]], exps(1).asInstanceOf[Expr[RealSort]])
      case AST.ge => ctx.mkGe(exps(0).asInstanceOf[Expr[RealSort]], exps(1).asInstanceOf[Expr[RealSort]])
      case AST.ite => ctx.mkITE(exps(0).asInstanceOf[Expr[BoolSort]], exps(1), exps(2))
      case _ => throw new Errors.InternalException(s"Incompatible operator $op in buildZ3ExpressionfromApp", panic = true)
    }
  }

  /**
    * Produces a Z3 expression representing a [[AST.Expression]].
    * Takes care of using the default values of [[AST.Offset]] expressions if its offset is outside the valid range.
    *
    * @param ex The [[AST.Expression]] represented by the Z3 expression
    * @param ts The timestamp the expressions applies to
    * @param maxTs The final timestamp
    * @param ctx The Z3 context
    * @return The new Z3 expression
    */
  def mkZ3Expr(ex: Expression, ts: Int, maxTs: Option[Int], ctx: Context): Expr[_] = {
    ex match {
      case AST.Application(op, exps) => Z3Utilities.mkZ3ExpressionfromApp(op, exps.map(mkZ3Expr(_, ts, maxTs, ctx)), ctx)
      case AST.Offset(id, 0, _) => mkIdExpr(id, 0, ctx)
      case AST.Offset(_, 1, default) if maxTs.contains(ts) => mkValueExpr(default, ctx)
      case AST.Offset(id, 1, _) => mkIdExpr(id, 1, ctx)
      case AST.Offset(_, -1, default) if ts == 0 => mkValueExpr(default, ctx)
      case AST.Offset(id, -1, _) => mkIdExpr(id, -1, ctx)
      case AST.Constant(v) => mkValueExpr(v, ctx)
      case id: AST.Identifier => mkIdExpr(id, 0, ctx)
      case _ => throw new Errors.InternalException(s"mkZ3Expr called for unhandled expression type: $ex", true)
    }
  }

  /**
    * Produces a set of Z3 expressions representing an identifier limited by a real range.
    * If both bounds are defined the set contains two expressions (e.g. [2,5] results in {id >= 2, id <= 5}).
    * If a bound is infinite the corresponding expression is missing from the set.
    *
    * @param id The identifier limited by the range
    * @param range The (possibly infinte) range of reals id can assume
    * @param ctx The Z3 context
    * @return The set of up to two new boolean Z3 expressions
    */
  def mkRangeZ3Expressions(id: SupIdentifier, range: OInterval, ctx: Context): Set[Expr[BoolSort]] = {
    val (from, to) = range
    val const = ctx.mkRealConst(id.name)
    var s: Set[Expr[BoolSort]] = Set()

    if (from.isDefined) {
      s += ctx.mkGe(const, ctx.mkReal(from.get.toString))
    }
    if (to.isDefined) {
      s += ctx.mkLe(const, ctx.mkReal(to.get.toString))
    }

    s
  }

  /**
    * Produces a Z3 expression representing an identifier limited by a set of reals.
    * E.g. for options = {2,4,6} this results in "id == 3 or id == 4 or id == 6".
    *
    * @param id The identifier imited by the set
    * @param options The set of reals id can assume
    * @param ctx The Z3 context
    * @return The new Z3 expression
    */
  def mkRealSetZ3Expression(id: SupIdentifier, options: Set[Double], ctx: Context): Expr[BoolSort] = {
    val eqs = options.map(d => ctx.mkEq(ctx.mkRealConst(id.name), ctx.mkReal(d.toString)))
    ctx.mkOr(eqs.toSeq: _*)
  }

  /**
    * Checks whether two Z3 expressions are equivalent.
    * Done by checking whether the expression "!(exp1 <-> exp2)" is unsatisfiable.
    *
    * @param exp1 The first expression
    * @param exp2 The second expression
    * @param config A configuration
    * @return True if both expressions are equivalent, false if not
    */
  def checkEquivalence(exp1: Expr[BoolSort], exp2: Expr[BoolSort], config: Config): Boolean = {
    val ctx = config.ctx
    val slv = ctx.mkSolver()
    config.z3RuntimeCollector.start()
    val res = slv.check(ctx.mkNot(ctx.mkIff(exp1, exp2))) 
    config.z3RuntimeCollector.stop()
    res match {
      case Status.UNSATISFIABLE => true
      case Status.SATISFIABLE => false
      case Status.UNKNOWN => throw new Errors.Z3Exception(s"SAT check ended without result, for:\n$exp1\n <-/-> $exp2")
    }
  }

  /**
    * Rewrites a Z3 expression into an expression without certain variables by applying quantifier elimination on those variables. 
    *
    * @param exp The original Z3 expression
    * @param boundVars The set of variables to be eliminated
    * @param config A configuration
    * @param chunkSize The number of variables to be eliminated at a time. None means all at once.
    * @param qeTactic The used Z3 tactic for quantifier elimination. Default is "qe2", others usually perform worse.
    * @return
    */
  def quantifierElimination(exp: Expr[BoolSort], boundVars: Set[SupIdentifier], config: Config, chunkSize: Option[Int] = None, qeTactic: String = "qe2"): Expr[BoolSort] = {
    val ctx = config.ctx

    val simpStac = ctx.repeat(
      ctx.andThen(
        ctx.repeat(ctx.mkTactic("simplify"), 100),
        ctx.repeat(ctx.mkTactic("propagate-ineqs"), 100),
        //ctx.repeat(ctx.mkTactic("solve-eqs"), 100),
        ctx.repeat(ctx.mkTactic("simplify"), 100),
        ctx.repeat(ctx.mkTactic("ctx-solver-simplify"), 100),
        ctx.repeat(ctx.mkTactic("simplify"), 100)
      ),
      1)

    val qeStac = ctx.andThen(
      ctx.repeat(ctx.mkTactic("simplify"), 100), // remove unnecessary quantifiers
      ctx.mkTactic(qeTactic)
    )

    // simplifies the expression to reduce complexity
    def simplify(exp: Expr[BoolSort]): Expr[BoolSort] = {
      val simpGoal = ctx.mkGoal(false, false, false)
      simpGoal.add(exp)
      simpStac.apply(simpGoal).getSubgoals.head.AsBoolExpr()
    }

    // performs a single quantifier elimination an a set of variables
    def subQE(exp: Expr[BoolSort], boundVars: Set[SupIdentifier]): Expr[BoolSort] = {
      val simplifiedExp = simplify(exp)
      Logging.log("After simplify: " + simplifiedExp.toString(), channel = Logging.DEBUG)

      val boundsArr = boundVars.map(Z3Utilities.mkVariableExpression(_, ctx)).toArray
      val exFormula = if (boundVars.isEmpty) simplifiedExp else ctx.mkExists(boundsArr, simplifiedExp, 1, null, null, null, null)

      val qeGoal = ctx.mkGoal(false, false, false)
      qeGoal.add(exFormula)
      val res = qeStac.apply(qeGoal).getSubgoals.head.AsBoolExpr()
      Logging.log("After QE: " + res.toString(), channel = Logging.DEBUG)

      res
    }

    Logging.log("Beginn iterative QE.", channel = Logging.DEBUG)
    
    config.z3RuntimeCollector.start()
    val res = if (boundVars.isEmpty) simplify(exp) else boundVars.grouped(chunkSize.getOrElse(boundVars.size)).foldLeft(exp) {
      case (currExp, boundChunk) => 
        Logging.log(s"Perform QE for $boundChunk on $currExp", channel = Logging.DEBUG)
        subQE(currExp, boundChunk)
    }
    config.z3RuntimeCollector.stop()
    
    Logging.log("Finished iterative QE.", channel = Logging.DEBUG)
    res
  }

}
