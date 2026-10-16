package unaa_lola.SymbolicEvaluation.z3Engine

import com.microsoft.z3.{AlgebraicNum, BoolSort, Context, Expr, IntNum, Optimize, RatNum, RealSort, Status}
import unaa_lola.AST.{Expression, SupIdentifier}
import unaa_lola.SymbolicEvaluation.z3Engine.RealAddition
import unaa_lola.util.Common.OInterval
import unaa_lola.util.{Common, Errors, Logging}
import unaa_lola.util.RuntimeCollector

/**
  * Object containing utility functions used during querying.
  */
object Z3CombiQuery {

  /**
   * Converts/evaluates a z3 expression to a real if possible.
   * 
   * @param exp The z3 expression to be evaluated
   * @return Some(value) if exp evaluates to a double, None otherwise
   */
  def doubleFromZ3Expression(exp: Expr[_]): Option[Double] = {
    def fromRatNum(r: RatNum): Double = (BigDecimal(r.getBigIntNumerator) / BigDecimal(r.getBigIntDenominator)).toDouble

    try {
      exp match {
        case i: IntNum => Some(i.getInt64.toDouble)
        case r: RatNum => Some(fromRatNum(r))
        case n: AlgebraicNum => Some(fromRatNum(n.toLower(10)))
        case _ =>
          Logging.log(s"Z3Query:fromRatNum called with expression that cannot be handled: $exp", channel = Logging.DEBUG)
          None
      }
    } catch {
      case t: Throwable =>
        Logging.log(s"Z3Query:fromRatNum raised exception (which is ignored)", addInfo = Some(exp.toString), throwable = Some(t),  channel = Logging.DEBUG)
        None
    }
  }

  /**
   * Simplify z3 term under assumption epsilon = 0.
   * 
   * @param exp The z3 expression to be simplified
   * @param ctx The current z3 context
   * @return The simplified expression
   */
  private def ignoreEpsilon(exp: Expr[_], ctx: Context): Expr[_] = {
    exp.substitute(ctx.mkConst("epsilon", ctx.mkRealSort()), ctx.mkReal(0)).simplify()
  }

}

/**
  * Class for querying expressions over a constraint state.
  * Used for determining the possible values an identifier can take.
  * Caches already computed results to improve performance.
  *
  * @param state The constraint state
  */
class Z3CombiQuery(state: Z3CombiConstraintState) {
  val z3RuntimeCollector = state.config.z3RuntimeCollector

  private val ranges = state.linAlgCore.ranges

  private val ctx = state.ctx
  private val slv = ctx.mkSolver()
  slv.add(state.z3Rep)

  // caches for boolean verdicts and real bounds
  private var boolVerdictCache: Map[Expr[BoolSort], Option[Boolean]] = Map()
  private var z3BoundCache: Map[Expr[RealSort], OInterval] = Map()
  private var rangeBoundCache: Map[RealAddition, OInterval] = Map()

  // returns an optimizer over the constaint state and the ranges from the LAC
  private def getOptimizer: Optimize = {
    val opt = ctx.mkOptimize()
    opt.Assert(state.z3Rep)
    ranges.foreach { case (id, range) => Z3Utilities.mkRangeZ3Expressions(id, range, ctx).foreach(ass => opt.Assert(ass)) }
    opt
  }

  /**
    * Queries a bool expression.
    *
    * @param exp The z3 bool expression
    * @return Some(false) if unsatisfiable, Some(true) if tautology, None if neither
    */
  def boolQuery(exp: Expr[BoolSort], addCond: Option[Expr[BoolSort]] = None): Option[Boolean] = {
    // TODO fix: if this function is called for the same exp multiple times but with different specialConds, the cache still triggers
    // TODO also do this properly without creating a new solver
    val tmpslv = ctx.mkSolver()
    tmpslv.add(state.z3Rep)
    addCond.foreach(tmpslv.add(_))

    if (!boolVerdictCache.contains(exp)) {
      z3RuntimeCollector.start()
      if (tmpslv.check(exp) == Status.UNSATISFIABLE) {
        boolVerdictCache += (exp -> Some(false))
      } else if (tmpslv.check(ctx.mkNot(exp)) == Status.UNSATISFIABLE) {
        boolVerdictCache += (exp -> Some(true))
      } else {
        boolVerdictCache += (exp -> None)
      }
      z3RuntimeCollector.stop()
    }

    boolVerdictCache(exp)
  }

  /**
    * Queries a real expression.
    *
    * @param exp The z3 real expression
    * @param specialCond An optional temporary additional assumption (used for linked streams)
    * @return The (possibly infinite) upper and lower bounds
    */
  def z3BoundQuery(exp: Expr[RealSort], addCond: Option[Expr[BoolSort]] = None): OInterval = {
    if (state.config.noZ3BoundQuery) {
      Logging.log(s"z3BoundQuery called for $exp but not switched on", channel = Logging.DEBUG)
      return (None, None)
    }

    Logging.log(s"z3BoundQuery called for $exp under $addCond", channel = Logging.DEBUG)

    // TODO fix: if this function is called for the same exp multiple times but with different specialConds, the cache still triggers
    if (!z3BoundCache.contains(exp)) {

      Logging.log(s"Not in cache!", channel = Logging.DEBUG)

      def checkOptResult(s: Status, o: Optimize): Unit = s match {
        case Status.UNSATISFIABLE => throw new Errors.Z3OptimizationException(s, o.getReasonUnknown, panic = addCond.isEmpty, debugOnly = addCond.isDefined)
        case Status.UNKNOWN => throw new Errors.Z3OptimizationException(s, o.getReasonUnknown)
        case Status.SATISFIABLE =>
      }

      Logging.log(s"z3BoundQuery: $exp", channel = Logging.DEBUG)

      val minOpt = getOptimizer
      val maxOpt = getOptimizer
      addCond.foreach(minOpt.Assert(_))
      addCond.foreach(maxOpt.Assert(_))
      val min = minOpt.MkMinimize(exp)
      val max = maxOpt.MkMaximize(exp)
      
      z3RuntimeCollector.start()
      val minResult = minOpt.Check()
      val maxResult = maxOpt.Check()
      z3RuntimeCollector.stop()

      checkOptResult(minOpt.Check(), minOpt)
      checkOptResult(maxOpt.Check(), maxOpt)
      Logging.log(s"-> min: $min", channel = Logging.DEBUG)
      Logging.log(s"-> max: $max", channel = Logging.DEBUG)
      
      z3BoundCache += (exp -> (
        Z3CombiQuery.doubleFromZ3Expression(Z3CombiQuery.ignoreEpsilon(min.getLower, ctx)),
        Z3CombiQuery.doubleFromZ3Expression(Z3CombiQuery.ignoreEpsilon(max.getUpper, ctx))
      ))

    }

    z3BoundCache(exp)
  }

  /**
    * Queries a [[RealAddition]].
    *
    * @param ra The [[RealAddition]]
    * @return The (possibly infinite) upper and lower bounds
    */
  def rangeBoundQuery(ra: RealAddition): OInterval = {
    val RealAddition(coeff, offset) = ra

    def sumUp(curr: Option[Double], id: SupIdentifier, c: Double, min: Boolean): Option[Double] = {
      curr.flatMap { curr =>
        if ((c < 0 && min) || (c >= 0 && !min)) {
          if (ranges.contains(id)) ranges(id)._2.map(v => curr + c * v) else None
        } else {
          if (ranges.contains(id)) ranges(id)._1.map(v => curr + c * v) else None
        }
      }
    }

    Logging.log(s"rangeBoundQuery called for $ra", channel = Logging.DEBUG)

    if (!rangeBoundCache.contains(ra)) {

      Logging.log(s"Not in cache!", channel = Logging.DEBUG)

      val min = coeff.foldLeft[Option[Double]](Some(offset)) { case (curr, (id, c)) => sumUp(curr, id, c, min = true) }
      val max = coeff.foldLeft[Option[Double]](Some(offset)) { case (curr, (id, c)) => sumUp(curr, id, c, min = false) }

      rangeBoundCache += (ra -> (min, max))
    }

    rangeBoundCache(ra)
  }

  /**
    * Queries a real expression that is described by both, a z3 expression and a [[RealAddition]].
    * Queries both individually and intersects their bounds.
    *
    * @param exp The z3 real expression
    * @param ra The [[RealAddition]]
    * @param addCond An optional temporary additional assumption (used for linked streams)
    * @return The (possibly infinite) upper and lower bounds
    */
  def combinedBoundQuery(exp: Expr[RealSort], ra: RealAddition, addCond: Option[Expr[BoolSort]] = None): OInterval = {
    val rangeBound = rangeBoundQuery(ra)

    val z3Bound = try {
      if (rangeBound._1.isEmpty || rangeBound._1 != rangeBound._2) z3BoundQuery(exp, addCond) else rangeBound
    } catch {
      case _: Errors.Z3OptimizationException => rangeBound
      case t: Throwable => throw new Errors.UnexpectedException(t, panic = true)
    }

    Logging.log(s"rangeBound: $rangeBound", channel = Logging.DEBUG)
    Logging.log(s"z3Bound: $z3Bound", channel = Logging.DEBUG)

    val res = Common.intersectIntervals(rangeBound, z3Bound)

    Logging.log(s"Query result $res", channel = Logging.DEBUG)
    res

  }

}
