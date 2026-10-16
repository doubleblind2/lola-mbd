package unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine

import com.microsoft.z3.{AST => _, _}
import unaa_lola.AST
import unaa_lola.AST.{SupIdentifier, Identifier}
import unaa_lola.Value.{Bool, Real, RealValue => NewRealValue, BoolValue => NewBoolValue, Value => NewValue}
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.POValue.{Value, BoolVal, UncertainBoolValue, RealVal, UncertainRealValue}
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.{Formula, QueryEnvironment}
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.LinAlg.{Z3LinAlgFormula, Z3LinAlgFormulaFactory}
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.Logical.{Z3LogicalFormula, Z3LogicalFormulaFactory}
import unaa_lola.util.Errors.Z3OptimizationException
import unaa_lola.util.{Common, Errors, Logging}

import scala.collection.mutable
import unaa_lola.Value.BoolValue
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.PastOnlyZ3Evaluator.{ReplacementIdentifier, newValToPoVal, poValToNewVal, CarrierValue}
import unaa_lola.Config

/**
 * Object with common constant operations useful for Z3 interaction
 */
object Z3QueryEnvironment {

  /**
   * Build z3 formula from value table
   * @param vars Variables to be involved in the formula
   * @param table Value table. The Bigint is interpreted bitwise, every bit encodes a combination of the vars
   *              Example: vars = a,b / table = 0b1100 => (a && b) || (!a && b)
   * @param ctx The current z3 context
   * @return A z3 [[BoolExpr]] encoding the value table
   */
  def buildZ3Expression(vars: Seq[SupIdentifier], table: BigInt, ctx: Context): BoolExpr = {
    val z3Vars = vars.map{Z3QueryEnvironment.buildZ3Expression(_, ctx).asInstanceOf[BoolExpr]}

    def buildAndTerm(c: Int): BoolExpr = {
      ctx.mkAnd(z3Vars.zipWithIndex.map {
        case (v, i) => if ((c >> i) % 2 == 1) v else ctx.mkNot(v)
      }:_*)
    }

    var tmpVal = table
    var retExp = ctx.mkFalse()

    (0 until Math.pow(2, vars.size).toInt).foreach{
      i =>
        retExp = if (tmpVal % 2 == 1) {
          ctx.mkOr(retExp, buildAndTerm(i))
        } else {
          retExp
        }
        tmpVal >>= 1
    }

    retExp
  }

  /**
   * Build z3 formula from an AST expression
   * @param e AST expression
   * @param ctx The current z3 context
   * @return A z3 [[Expr]] encoding the AST expression
   */
  def buildZ3Expression(e: AST.Expression, ctx: Context): Expr[_] = {
    e match {
      case AST.Application(op, exps) =>
        val newExps = exps.map(buildZ3Expression(_, ctx))

        op match {
          case AST.eq => ctx.mkEq(newExps(0), newExps(1))
          case AST.ne => ctx.mkNot(ctx.mkEq(newExps(0), newExps(1)))
          case AST.and => ctx.mkAnd(newExps(0).asInstanceOf[BoolExpr], newExps(1).asInstanceOf[BoolExpr])
          case AST.or => ctx.mkOr(newExps(0).asInstanceOf[BoolExpr], newExps(1).asInstanceOf[BoolExpr])
          case AST.not => ctx.mkNot(newExps(0).asInstanceOf[BoolExpr])
          case AST.iff => ctx.mkIff(newExps(0).asInstanceOf[BoolExpr], newExps(1).asInstanceOf[BoolExpr])
          case AST.imp => ctx.mkImplies(newExps(0).asInstanceOf[BoolExpr], newExps(1).asInstanceOf[BoolExpr])
          case AST.plus => ctx.mkAdd(newExps(0).asInstanceOf[ArithExpr[_]], newExps(1).asInstanceOf[ArithExpr[_]])
          case AST.times => ctx.mkMul(newExps(0).asInstanceOf[ArithExpr[_]], newExps(1).asInstanceOf[ArithExpr[_]])
          case AST.lt => ctx.mkLt(newExps(0).asInstanceOf[ArithExpr[_]], newExps(1).asInstanceOf[ArithExpr[_]])
          case AST.gt => ctx.mkGt(newExps(0).asInstanceOf[ArithExpr[_]], newExps(1).asInstanceOf[ArithExpr[_]])
          case AST.le => ctx.mkLe(newExps(0).asInstanceOf[ArithExpr[_]], newExps(1).asInstanceOf[ArithExpr[_]])
          case AST.ge => ctx.mkGe(newExps(0).asInstanceOf[ArithExpr[_]], newExps(1).asInstanceOf[ArithExpr[_]])
        }
      case AST.Constant(v) => v match {
        case NewBoolValue(_) | NewRealValue(_) => buildZ3ExpressionFromValue(newValToPoVal(v), ctx)
        case CarrierValue(poV) => buildZ3ExpressionFromValue(poV, ctx)
      }
      case AST.Identifier(name, typ) =>
        typ match {
          case Real => ctx.mkRealConst(name)
          case Bool => ctx.mkBoolConst(name)
        }
      case ReplacementIdentifier(_, _, op, vals) => buildZ3Expression(AST.Application(op, vals.map(CarrierValue).map(AST.Constant)), ctx)
      case _ => throw new Errors.Z3Exception(s"Cannot build z3 expression from $e")
    }
  }



  /**
   * Build z3 formula from an AST value
   * @param v AST value
   * @param ctx The current z3 context
   * @return A z3 [[Expr]] encoding the AST expression
   */
  def buildZ3ExpressionFromValue(v: Value, ctx: Context) : Expr[_] = {
    v match {
      case RealVal(value) => ctx.mkReal(value.toString)
      case BoolVal(value) => ctx.mkBool(value)
      case UncertainRealValue(formula) => buildZ3ExpressionFromFormula(formula, ctx)
      case UncertainBoolValue(formula) => buildZ3ExpressionFromFormula(formula, ctx)
      case _ => throw new Errors.InternalException(s"Cannot build z3 expression from value $v")
    }
  }

  /**
   * Build z3 formula from abstract [[Formula]] (onluy supported for [[Z3LinAlgFormula]] and [[Z3LogicalFormula]]
   * @param f Formula
   * @param ctx The current z3 context
   * @return A z3 [[Expr]] encoding the formula
   */
  def buildZ3ExpressionFromFormula(f: Formula[_], ctx: Context) : Expr[_] = {
    f match {
      case f: Z3LinAlgFormula => ctx.mkAdd(ctx.mkAdd(f.coeff.map{case (id, c) => ctx.mkMul(ctx.mkReal(c.toString), ctx.mkRealConst(id.name))}.toSeq:_*), ctx.mkReal(f.offset.toString))
      case f: Z3LogicalFormula => f.z3Shortcut
      case _ => throw new Errors.InternalException(s"Cannot build z3 expression from formula $f")
    }
  }

  /**
   * Convert/evaluate z3 expression to double if possible
   * @param exp z3 expression to be evaluated
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
          Logging.log(s"Z3Query:fromRatNum called with expression that cannot be handled: $exp", Logging.WARNING)
          None
      }
    } catch {
      case t: Throwable =>
        Logging.log(s"Z3Query:fromRatNum raised exception (which is ignored)", Logging.WARNING, Some(exp.toString), Some(t))
        None
    }
  }

  /**
   * Simplify z3 term under assumption epsilon = 0
   * @param exp z3 expression to be simplified
   * @param ctx The current z3 context
   * @return Simplified expression
   */
  private def ignoreEpsilon(exp: Expr[_], ctx: Context): Expr[_] = {
    exp.substitute(ctx.mkConst("epsilon", ctx.mkRealSort()), ctx.mkReal(0)).simplify()
  }

}

/**
 * Main class for handling interaction with z3
 * @param ctx The active z3 context
 */
class Z3QueryEnvironment(config: Config) extends QueryEnvironment {

  val ctx = config.ctx

  private var ranges : Map[AST.Identifier, (Option[Double], Option[Double])] = Map()
  private var assumptions: Set[BoolExpr] = Set()

  private val rangeCache: mutable.Map[Z3LinAlgFormula, (Option[Double], Option[Double])] = mutable.Map()
  private val verdictCache: mutable.Map[BoolExpr, Option[Boolean]] = mutable.Map()

  private val logicalFormulaFactory = new Z3LogicalFormulaFactory(this)
  private val linAlgFormulaFactory = new Z3LinAlgFormulaFactory(this)

  /**
   * Get factory for [[Z3LogicalFormula]] compatible for this [[Z3QueryEnvironment]]
   * @return Factory for z3 logical formulas
   */
  def getLogicalFormulaFactory : Z3LogicalFormulaFactory = logicalFormulaFactory

  /**
   * Get factory for [[Z3LinAlgFormula]] compatible for this [[Z3QueryEnvironment]]
   * @return Factory for z3 linear algebra formulas
   */
  def getLinAlgFormulaFactory : Z3LinAlgFormulaFactory = linAlgFormulaFactory


  /** @inheritdoc */
  override def addAssumption(v: Option[Value]): Unit = {
      v.foreach {
        case BoolVal(value) => assumptions += ctx.mkBool(value)
        case UncertainBoolValue(value: Z3LogicalFormula) => assumptions += value.z3Shortcut
        case _ => throw new Errors.InternalException(s"Cannot add assumption which is no z3 compatible value: $v")
      }
  }

  /** @inheritdoc */
  override def clearAssumptions(): Unit = {
    assumptions = Set()
  }

  /** @inheritdoc */
  override def addIftoAssumption(id: Identifier, cond: Value, thenExp: Value, elseExp: Value): Unit = {
    val condE = Z3QueryEnvironment.buildZ3ExpressionFromValue(cond, ctx).asInstanceOf[BoolExpr]
    val thenE = Z3QueryEnvironment.buildZ3ExpressionFromValue(thenExp, ctx)
    val elseE = Z3QueryEnvironment.buildZ3ExpressionFromValue(elseExp, ctx)
    val idE = Z3QueryEnvironment.buildZ3Expression(id, ctx)
    val iteE = ctx.mkAnd(ctx.mkImplies(condE, ctx.mkEq(idE, thenE)), ctx.mkImplies(ctx.mkNot(condE), ctx.mkEq(idE, elseE)))

    assumptions += iteE
  }

  /** @inheritdoc */
  override def addRange(id: Identifier, from: Option[Double], to: Option[Double]): Unit = {
    ranges += (id -> (from, to))
  }

  /** @inheritdoc */
  override def clearRanges(): Unit = {
    ranges = Map()
  }

  /** @inheritdoc */
  override def clearRangesExceptFrom(activeIDs: Set[Identifier]): Unit = {
    val oldRangeSize = ranges.size
    ranges --= (ranges.keys.toSet -- activeIDs)
    Logging.log(s"Ranges cleared; Now keeping ${ranges.size} ranges (previously: ${oldRangeSize})!", channel = Logging.DEBUG)
  }

  /**
   * Build set of z3 [[BoolExpr]] encoding all current assumptions and ranges
   * @return  Set of z3 [[BoolExpr]] encoding all current assumptions and ranges
   */
  private def getCurrentZ3Constraints: Set[BoolExpr] = {
    var rs = Set[BoolExpr]()
    ranges.foreach{
        case (id, (from, to)) =>
          val c = ctx.mkRealConst(id.name)

          if (from.isDefined && to.isDefined && from.get == to.get) {
            rs += ctx.mkEq(c, Z3QueryEnvironment.buildZ3Expression(AST.Constant(NewRealValue(from.get)), ctx).asInstanceOf[ArithExpr[_]])
          } else {

            if (from.isDefined) {
              rs += ctx.mkGe(c, Z3QueryEnvironment.buildZ3Expression(AST.Constant(NewRealValue(from.get)), ctx).asInstanceOf[ArithExpr[_]])
            }

            if (to.isDefined) {
              rs += ctx.mkLe(c, Z3QueryEnvironment.buildZ3Expression(AST.Constant(NewRealValue(to.get)), ctx).asInstanceOf[ArithExpr[_]])
            }
          }
      }

    rs ++ assumptions
  }

  private def getStandardSlv: Solver = {
    val slv = ctx.mkSolver()
    getCurrentZ3Constraints.foreach{const => slv.add(const)}
    slv
  }

  private def getStandardOpt: Optimize = {
    val opt = ctx.mkOptimize()
    getCurrentZ3Constraints.foreach{const => opt.Assert(const)}
    opt
  }



  /**
   * Check if a z3 [[BoolExpr]] is satisfiable/tautology given the current assumptions/ranges
   * @param exp Expression to be checked
   * @return Some(true): expression is a tautology, Some(false): expression is unsatisfiable, None: Satisfiable or check did not bring a result
   */
  def checkConformance(exp: BoolExpr): Option[Boolean] = {

    if (!verdictCache.contains(exp)) {
      val tmpSlv = getStandardSlv

      config.z3RuntimeCollector.start()
      if (tmpSlv.check(exp) == Status.UNSATISFIABLE) {
        verdictCache += (exp -> Some(false))
      } else if (tmpSlv.check(ctx.mkNot(exp)) == Status.UNSATISFIABLE) {
        verdictCache += (exp -> Some(true))
      } else {
        verdictCache += (exp -> None)
      }
      config.z3RuntimeCollector.stop()
    }

    verdictCache(exp)
  }

  /**
   * Check if a combination of [[BoolExpr]] is satisfiable/tautology given the current assumptions/ranges
   * @param lfs Sequence of expressions to be examined
   * @param valuation Binary encoding if expressions from lfs must hold (bit is set to 1) or their opposite must hold (bit set to 0)
   * @param assump Further (temporary) assumption as z3 [[BoolExpr]]
   * @return false: Combination is not satisfiable, true: Combination is satisfiable
   */
  def checkConformance(lfs: Seq[BoolExpr], valuation: Int, assump: BoolExpr): Boolean = {
    var tmpVal = valuation
    val tmpSlv = getStandardSlv

    lfs.foreach {
      e: BoolExpr =>
        val exp = if (tmpVal % 2 == 1) e else ctx.mkNot(e)
        tmpSlv.add(exp)
        tmpVal = tmpVal >> 1
    }

    tmpSlv.add(assump)

    config.z3RuntimeCollector.start()
    val res = tmpSlv.check() 
    config.z3RuntimeCollector.stop()
    
    res match {
      case Status.UNSATISFIABLE => false
      case Status.SATISFIABLE => true
      case Status.UNKNOWN => throw new Errors.Z3Exception(s"SAT check ended without result, for:\n$lfs")
    }
  }


  /**
   * Query possible bounds of single real identifier
   * @param id Identifier to be checked
   * @return Tuple of possible lower or upper bound (no tight bounds, can be over approximations)
   */
  def boundQuery(id: Identifier): (Option[Double], Option[Double]) = {
    boundQuery(Z3LinAlgFormula(Map(id -> 1), 0, this))
  }

  /**
   * Query possible bounds of a [[Z3LinAlgFormula]]
   * @param laf Formula to be checked
   * @return Tuple of possible lower or upper bound (no tight bounds, can be over approximations)
   */
  def boundQuery(laf: Z3LinAlgFormula): (Option[Double], Option[Double]) = {

    
    Logging.log(s"Bound query triggered for $laf", channel = Logging.DEBUG)
    if (!rangeCache.contains(laf)) {
      Logging.log(s"Not in cache!", channel = Logging.DEBUG)
      val rangeBound = rangeBoundQuery(laf)

      val z3Bound = try {
         if (rangeBound._1.isEmpty || rangeBound._1 != rangeBound._2) z3BoundQuery(Z3QueryEnvironment.buildZ3ExpressionFromFormula(laf, ctx)) else rangeBound
      } catch {
        case _: Z3OptimizationException => rangeBound
        case t: Throwable => throw new Errors.UnexpectedException(t, panic = true)
      }

      Logging.log(s"rangeBound: $rangeBound", channel = Logging.DEBUG)
      Logging.log(s"z3Bound: $z3Bound", channel = Logging.DEBUG)

      rangeCache += (laf -> Common.intersectIntervals(rangeBound, z3Bound))
    }

    Logging.log(s"Query result ${rangeCache(laf)}", channel = Logging.DEBUG)
    rangeCache(laf)
  }

  private def rangeBoundQuery(laf: Z3LinAlgFormula): (Option[Double], Option[Double]) = {
    def sumUp(curr: Option[Double], id: Identifier, c: Double, min: Boolean) : Option[Double] = {

      curr.flatMap{curr =>
        if ((c < 0 && min) || (c >= 0 && !min)) {
          if (ranges.contains(id)) ranges(id)._2.map(v => curr + c * v) else None
        } else {
          if (ranges.contains(id)) ranges(id)._1.map(v => curr + c * v) else None
        }
      }
    }

  val min = laf.coeff.foldLeft[Option[Double]](Some(laf.offset)){case (curr, (id, c)) => sumUp(curr, id, c, min = true)}
  val max = laf.coeff.foldLeft[Option[Double]](Some(laf.offset)){case (curr, (id, c)) => sumUp(curr, id, c, min = false)}

  (min, max)
  }


  private def z3BoundQuery(exp: Expr[_]): (Option[Double], Option[Double]) = {

    if (config.noZ3BoundQuery) {
      Logging.log(s"z3BoundQuery called for $exp but not switched on", channel = Logging.DEBUG)
      return (None, None)
    }

    def checkOptResult(s: Status, o: Optimize) : Unit = s match {
      case Status.UNSATISFIABLE => throw new Errors.Z3OptimizationException(s, o.getReasonUnknown, panic = true)
      case Status.UNKNOWN => throw new Errors.Z3OptimizationException(s, o.getReasonUnknown)
      case Status.SATISFIABLE =>
    }

    val optMin = getStandardOpt
    val optMax = getStandardOpt

    val max = optMax.MkMaximize(exp)
    val min = optMin.MkMinimize(exp)

    config.z3RuntimeCollector.start()
    val minResult = optMin.Check()
    val maxResult = optMax.Check()
    config.z3RuntimeCollector.stop()

    checkOptResult(minResult, optMin)
    checkOptResult(maxResult, optMax)
    

    Logging.log(s"z3BoundQuery: $exp", channel = Logging.DEBUG)
    Logging.log(s"-> min: $min", channel = Logging.DEBUG)
    Logging.log(s"-> max: $max", channel = Logging.DEBUG)

    (
      Z3QueryEnvironment.doubleFromZ3Expression(Z3QueryEnvironment.ignoreEpsilon(min.getLower, ctx)),
      Z3QueryEnvironment.doubleFromZ3Expression(Z3QueryEnvironment.ignoreEpsilon(max.getUpper, ctx))
    )

  }

  /** @inheritdoc */
  override def resetCaches(): Unit = {
    rangeCache.clear()
    verdictCache.clear()
  }

}
