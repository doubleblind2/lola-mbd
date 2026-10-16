package unaa_lola.SymbolicEvaluation.z3Engine

import com.microsoft.z3._
import unaa_lola.AST.{Expression, Specification, SupIdentifier, SymbolIdentifier}
import unaa_lola.SymbolicEvaluation.ConstraintState
import unaa_lola.util.Common.OInterval
import unaa_lola.util.{Common, Errors, Logging}
import unaa_lola.AST
import unaa_lola.Value._
import unaa_lola.Config

/**
  * Object containing a function for generating empty constraint states.
  */
object Z3CombiConstraintState {

  /**
    * Returns the empty state for a specification.
    * A timestamp of -1 shows its emptyness.
    *
    * @param spec The specification
    * @param config A configuration
    * @return The empty state
    */
  def getEmptyState(spec: Specification, config: Config): ConstraintState = {
    Z3CombiConstraintState(-1, Set(), LinAlgCore.empty(config), new QuickCalcCore(spec), None, spec, config)
  }

}

/**
  * Concrete implmentation of [[ConstraintState]] with SMT solver z3 as backend.
  *
  * @param ts The current timestamp
  * @param constraints The set of constraints, i.e. set of z3 expressions
  * @param linAlgCore The [[LinAlgCore]] for this state
  * @param qcc The [[QuickCalcCore]] for this state
  * @param future Optionally a [[Future]] to further constrain values referenced by +1-offsets
  * @param spec The flattened specification
  * @param config A configuration
  */
case class Z3CombiConstraintState(override val ts: Int, constraints: Set[Expr[BoolSort]], linAlgCore: LinAlgCore, qcc: QuickCalcCore, future: Option[Z3CombiConstraintState], spec: AST.Specification, config: Config) extends ConstraintState(ts) {

  val ctx = config.ctx

  // Sets of all stream identifiers (input + non-input) with specific timestamps
  //TODO maybe make these lazy
  val actualKeep = if (config.keepPastSymbolsFor > ts) ts else config.keepPastSymbolsFor
  private val identifierMinusUntilKeep = spec.identifiers.flatMap(i => Range(-actualKeep, 0).map(SymbolIdentifier(i,_)))
  private val identifierZero = spec.identifiers.map(SymbolIdentifier(_, 0))
  private val identifierPlus1 = spec.identifiers.map(SymbolIdentifier(_, 1))

  // All existing identifiers for real streams
  //private val realSymbols = spec.identifiers.flatMap(i => Range(-actualKeep, 2).map(SymbolIdentifier(i,_))).filter(_.typ == Real)
  private val realSymbols = (identifierMinusUntilKeep ++ identifierZero ++ identifierPlus1).filter(_.typ == Real)

  private var query: Option[Z3CombiQuery] = None

  /**
    * The z3 expression representing this state including the future, i.e. the z3 representant of the next step.
    * (theoretically constraints could be empty while the future is not but practically no values for the current timestamp would be computed then)
    */
  lazy val z3Rep: Expr[BoolSort] = if (constraints.isEmpty) ctx.mkTrue() else ctx.mkAnd(constraints.reduce((a, b) => ctx.mkAnd(a, b)), future.map(_.timeShift(-1).z3Rep).getOrElse(ctx.mkTrue()))

  /**
    * Retrieves the [[Z3CombiQuery]] for this state.
    * Will generate the query, if not already done.
    * 
    * @return The query for this state
    */
  def getQuery: Z3CombiQuery = {
    val q = query.getOrElse(new Z3CombiQuery(this))
    query = Some(q)
    q
  }

  /** @inheritdoc */
  override def transformTs(to: Int): Z3CombiConstraintState = {
    qcc.nextTs()
    if (ts == -1) {
      copy(ts = to)
    } else {
      timeShift(to-ts)
    }
  }

  /**
    * Shifts the state by a number of timesteps, i.e. shifting every symbol s^t to s^t-o.
    *
    * @param o The offsets determining the new timestamp
    * @return The shifted state
    */
  def timeShift(o: Int, from: Int = -config.keepPastSymbolsFor, until: Int = 1): Z3CombiConstraintState = {
    // Generates a pair of two z3 expressions (id^f, id^t).
    def getExchanges(id: AST.Identifier, f: Int, t: Int): (Expr[_], Expr[_]) = {
      val (fid, tid) = (SymbolIdentifier(id, f), SymbolIdentifier(id, t))
      fid.baseId.typ match {
        //TODO use Z3Utilities function
        case Real => (ctx.mkRealConst(fid.toString), ctx.mkRealConst(tid.toString))
        case Bool => (ctx.mkBoolConst(fid.toString), ctx.mkBoolConst(tid.toString))
      }
    }

    // Exchanges in exps every occurrence of id^r by id^r-o.
    def doExchange(exps: Set[Expr[BoolSort]], id: AST.Identifier, r: Int): Set[Expr[BoolSort]] = {
      val (e1, e2) = getExchanges(id, r, r - o)
      exps.map(e => e.substitute(e1, e2))
    }

    // The order is important here. If for negative offsets we shift 0-symbols first and then -1-symbols, the first ones end up shifted twice
    // For positive offsets it's the other way around
    val newC = (if (o > 0) Range(from, until+1) else Range(from, until+1).reverse).foldLeft(constraints) {
      case (cs, ts) => spec.identifiers.foldLeft(cs) {
        case (c, id) => doExchange(c, id, ts)
      }
    }

    copy(constraints = newC, ts = ts + o, linAlgCore = linAlgCore.timeShift(o))
  }

  // Eliminates ids from the constraints by utilizing quantifier elimination.
  private def variableElimination(ids: Set[SymbolIdentifier]): Z3CombiConstraintState = {
    val newConstraint = Z3Utilities.quantifierElimination(z3Rep, ids.asInstanceOf[Set[SupIdentifier]], config)
    copy(constraints = Set(newConstraint))
  }

  /** @inheritdoc */
  override def setTs(ts: Int): Z3CombiConstraintState = copy(ts = ts)

  /** @inheritdoc */
  override def simplify(kill: Set[AST.SymbolIdentifier], stage : Int, pred: Option[ConstraintState]): Z3CombiConstraintState = {
    Logging.log(s"Simplify called with stage $stage and z3Rep size ${z3Rep.toString.length}", channel = Logging.DEBUG)
    
    // determines up to which size (as in length of the z3 representation) certain stages should do nothing
    val sizeMap: Map[Int, Int] = Map(1 -> 3000, 2 -> 1000, 3 -> 500)

    // the vars to be spared during simplification
    val spare = ((identifierMinusUntilKeep ++ identifierZero ++ identifierPlus1) -- kill).asInstanceOf[Set[SupIdentifier]]

    def doBoolPruning() = variableElimination(kill ++ realSymbols)

    stage match {
      case i if (i <= 3 && i > 0 && (z3Rep.toString.length + linAlgCore.toString.length) > sizeMap(i)) || i == 4 =>
        // Quantifier elimination for Boolean vars only, pruning for reals if constraint size exceeds sizeMap
        val newCs = doBoolPruning()
        val newLac = linAlgCore.rewrite(spare, this)

        newCs.copy(linAlgCore = newLac, constraints = newCs.constraints + newLac.getZ3Representation(kill.asInstanceOf[Set[SupIdentifier]]))
      case 5 =>
        // Quantifier elimination for Boolean vars only, widened pruning for reals
        if (pred.isEmpty) {
          Logging.log("Stage 5 simplify (widening) called without previous state", sev = Logging.WARNING)
          return simplify(kill, 4, pred)
        }
        Logging.log("Apply widening.", channel = Logging.TECHNICAL_LOG)
        val newCs = doBoolPruning()
        val predCs = pred.get.asInstanceOf[Z3CombiConstraintState]
        val newLac = linAlgCore.widen(predCs, this)

        newCs.copy(linAlgCore = newLac, constraints = newCs.constraints + newLac.getZ3Representation(kill.asInstanceOf[Set[SupIdentifier]]))
      case i if i > 5 =>
        // Quantifier elimination for Boolean vars only
        doBoolPruning().copy(linAlgCore = LinAlgCore.maxAbstraction(spec, config))
      case _ =>
        // Only do clean up (includes case i = 0)
        cleanUp(kill)
    }
  }

  /** @inheritdoc */
  override def getValue(id: AST.SymbolIdentifier): Value = {
    val relTs = id.ts - ts
    val nid = SymbolIdentifier(id.baseId, relTs)

    if (qcc.get(id.baseId).isDefined && relTs == 0) {
      return qcc.get(id.baseId).get
    }
    if (qcc.getLast(id.baseId).isDefined && relTs == -1 && ts > 0) {
      return qcc.getLast(id.baseId).get
    }

    // Samples a range from start to end by rate. Generates a set whose values are spaced by rate.
    def sample(range: OInterval, start: Double, end: Double, rate: Double): RealSetValue = {
      val validOptions = Range.BigDecimal.inclusive(start, end, rate).map(_.toDouble).toSet.filter {
        d => 
          Common.intervalContains(range, d) &&
          copy(constraints = constraints + ctx.mkEq(ctx.mkRealConst(nid.toString), ctx.mkReal(d.toString))).isConsistent
      }
      RealSetValue(validOptions)
    }

    val addCond = if (spec.linkedStreams.isDefinedAt(id.baseId)) {
      val z3Exp = ctx.mkBoolConst(SymbolIdentifier(spec.linkedStreams(id.baseId), relTs).toString)
      if (getQuery.boolQuery(z3Exp) == Some(false)) {
        return InvalidRealValue
      } else {
        Some(z3Exp)
      }
    } else {
      None
    }

    id.typ match {
      case Real => // For reals obtain possible range, encode if actually an enum stream or sample if desired
        //val addCond = spec.linkedStreams.get(id.baseId).map(id => ctx.mkBoolConst(SymbolIdentifier(id, relTs).toString))
        val r = getQuery.combinedBoundQuery(ctx.mkRealConst(nid.toString), linAlgCore.getIDValue(id.baseId), addCond)
        spec.getEncodedEnum(id.baseId) match {
          case Some(e) => sample(r, 0, e.constants.length - 1, 1) // TODO maybe also make sure that no singleton sets are returned
          case None => spec.samplingRates.get(id.baseId) match {
            case Some((f, t, rate)) => sample(r, f, t, rate)            // TODO same as above
            case None => 
              if (r._1.isDefined && r._1 == r._2) {
                RealValue(r._1.get)
              } else {
                RangeValue(r._1, r._2, linAlgCore.getIDValue(id.baseId).toString)
              }
          }
        }
      case Bool => // For bools simply ask the query
        getQuery.boolQuery(ctx.mkBoolConst(nid.toString), addCond).map(BoolValue).getOrElse(UnknownBoolValue(nid.toString))
    }
  }

  /** @inheritdoc */
  override def addInput(id: AST.Identifier, input: Value): Z3CombiConstraintState = {
    val nid = SymbolIdentifier(id, 0)
    val z3Id = Z3Utilities.mkVariableExpression(nid, ctx)

    val newConst = input match {
      case BoolValue(v) =>
        qcc.input(id, input)
        constraints + ctx.mkEq(z3Id, ctx.mkBool(v))
      case RealValue(v) =>
        qcc.input(id, input)
        constraints + ctx.mkEq(z3Id, ctx.mkReal(v.toString))
      case RangeValue(from, to, _) => constraints ++ Z3Utilities.mkRangeZ3Expressions(nid, (from, to), ctx)
      case RealSetValue(options) => constraints + Z3Utilities.mkRealSetZ3Expression(nid, options, ctx)
      case UnknownBoolValue(_) => constraints
    }

    copy(constraints = newConst, linAlgCore = linAlgCore.addInput(id, input))
  }

  // Applies the spec once to this state to generate a new state.
  // Always generates constraints for timestamp 0. Therefore, all timestamps need to be shifted beforehand.
  // This function is used for both, forward and backward unrolling, depending on the preceeding time shift.
  private def unrollSpecZ3(spec: AST.Specification, maxTs: Option[Int]): Z3CombiConstraintState = {
    // Essentially the same function as in Z3Utilities but utilizing the QuickCalcCore if applicable.
    def mkZ3Expr(ex: Expression, ts: Int, maxTs: Option[Int], ctx: Context): Expr[_] = {
      ex match {
        case AST.Application(op, exps) => Z3Utilities.mkZ3ExpressionfromApp(op, exps.map(mkZ3Expr(_, ts, maxTs, ctx)), ctx)
        case id: AST.Identifier if qcc.get(id).isDefined => Z3Utilities.mkValueExpr(qcc.get(id).get, ctx)
        case AST.Offset(id, 0, _) if qcc.get(id).isDefined => Z3Utilities.mkValueExpr(qcc.get(id).get, ctx)
        case AST.Offset(id, -1, _) if ts != 0 && qcc.getLast(id).isDefined => Z3Utilities.mkValueExpr(qcc.getLast(id).get, ctx)
        case _ => Z3Utilities.mkZ3Expr(ex, ts, maxTs, ctx)
      }
    }

    val newConst = spec.definitions.flatMap {
      case (id, exp) => {
        if (qcc.get(id).isEmpty) {
          Some(ctx.mkEq(Z3Utilities.mkIdExpr(id, 0, ctx), mkZ3Expr(exp, ts, maxTs, ctx)))
        } else if (future.isDefined) {
          Some(ctx.mkEq(Z3Utilities.mkIdExpr(id, 0, ctx), mkZ3Expr(id, ts, maxTs, ctx)))
        } else {
          None
        }
      }
    }

    copy(constraints = constraints ++ newConst)
  }

  // Checks whether all constraints (including future) are satisfiable.
  private def isConsistent: Boolean = getQuery.boolQuery(ctx.mkTrue()).contains(true)

  // Forces the assumption stream (if defined) to be true and checks if the constraint state is consistent afterwards.
  private def checkConstraintsAndAddAssumption(): Z3CombiConstraintState = {
    val newCs = if (spec.definitions.contains(Common.assStream)) {
      copy(constraints = constraints + ctx.mkBoolConst(SymbolIdentifier(Common.assStream, 0).toString))
    } else {
      this
    }
    
    if (qcc.get(Common.assStream).contains(BoolValue(false)) || !newCs.isConsistent) {
      Logging.log(s"Assumption broken or constraint set inconsistent in state $this", channel = Logging.DEBUG)
      throw new Errors.InternalException("Assumption broken or constraint set inconsistent!")
    }

    newCs
  }

  /** @inheritdoc */
  override def applySpec(spec: AST.Specification, maxTs: Option[Int]): Z3CombiConstraintState = {
    qcc.applySpec(ts, maxTs)
    var newCs = unrollSpecZ3(spec, maxTs).checkConstraintsAndAddAssumption()
    newCs = newCs.copy(linAlgCore = newCs.linAlgCore.applySpecFwd(spec, newCs.ts, newCs, qcc))
    newCs.copy(linAlgCore = newCs.linAlgCore.improveInputRanges(newCs))
  }

  /** @inheritdoc */
  override def applySpecReversed(spec: AST.Specification, maxTs: Option[Int]): Z3CombiConstraintState = {
    val newCs = unrollSpecZ3(spec, maxTs).checkConstraintsAndAddAssumption()
    newCs.copy(linAlgCore = newCs.linAlgCore.applySpecBwd(spec, newCs.ts, newCs))
  }

  /** @inheritdoc */
  override def cleanUp(symbolsToKill: Set[AST.SymbolIdentifier]): Z3CombiConstraintState = {
    variableElimination(symbolsToKill)
  }

  /** @inheritdoc */
  override def setFuture(cs: ConstraintState): Z3CombiConstraintState = {
    copy(future = Some(cs.asInstanceOf[Z3CombiConstraintState]))
  }

  /** @inheritdoc */
  override def clearFuture(): Z3CombiConstraintState = {
    copy(future = None)
  }

  /** @inheritdoc */
  override def isCongruentTo(other: ConstraintState, ignore: Set[SymbolIdentifier] = Set()): Boolean = {
    val otherRep = other.asInstanceOf[Z3CombiConstraintState].z3Rep
    val res = Z3Utilities.checkEquivalence(z3Rep, otherRep, config)
    res && linAlgCore.isCongruentTo(other.asInstanceOf[Z3CombiConstraintState].linAlgCore, ignore = ignore.asInstanceOf[Set[SupIdentifier]])
  }

  /** @inheritdoc */
  override def toString: String =
    s"""
      |Z3CombiConstraintState(ts = $ts){
      |${constraints.mkString(",\n")}
      |-------
      |${future.map(_.constraints.mkString(",\n")).getOrElse("")}
      |#######
      |$linAlgCore
      |}
      |""".stripMargin
}
