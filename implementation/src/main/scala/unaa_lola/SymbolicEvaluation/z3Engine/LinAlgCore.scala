package unaa_lola.SymbolicEvaluation.z3Engine

import com.microsoft.z3.{BoolSort, Context, Expr}
import unaa_lola.AST
import unaa_lola.AST.{Expression, Identifier, Specification, SupIdentifier, SymbolIdentifier}
import unaa_lola.SymbolicEvaluation.z3Engine.RealAddition
import unaa_lola.SymbolicEvaluation.z3Engine.RealAddition.{realAdditionFromId, realAdditionFromValue, buildRealAdditionFromApp}
import unaa_lola.SymbolicEvaluation.z3Engine.Z3Utilities.mkRangeZ3Expressions
import unaa_lola.util.Common.OInterval
import com.microsoft.z3.RealSort
import unaa_lola.SymbolicEvaluation.z3Engine
import unaa_lola.util.Common
import unaa_lola.SymbolicEvaluation.z3Engine.Rewriting.Z3LinAlgOptimization
import unaa_lola.util.UnionFind
import unaa_lola.Value._
import unaa_lola.Config

/**
  * Object containing functions to generate new LACs.
  */
object LinAlgCore {

  /**
    * The empty LAC, i.e. with empty states, inputs and ranges.
    * @param config A configuration
    * @return The empty LAC
    */
  def empty(config: Config): LinAlgCore = LinAlgCore(Map(), Map(), Map(), Map(), config)

  /**
    * Returns a subset of the defined streams in a specification of type real.
    *
    * @param spec The specification
    * @return The real defined streams
    */
  def realDefs(spec: AST.Specification): Map[Identifier, AST.Expression] = spec.definitions.filter(_._1.getType == Real)

  /**
    * Returns a subset of the defined streams in a specification of type real that occurr as positive offsets.
    *
    * @param spec The specification
    * @return The real and positively-referenced defined streams
    */
  def realPlusDefs(spec: AST.Specification): Set[Identifier] = spec.crawlOffsets(1).filter(id => id.getType == Real && spec.definitions.contains(id))

  // Only applicable for future states during backwards calculation.
  // Otherwise realDefs + real inputs must be used
  /**
    * The maximally abstracted LAC for a specification. Used during simplification.
    * Both states of the LAC only contain formulas of the form id^0 for ids of defined streams that are positvely referenced.
    *
    * @param spec The specification
    * @return The maximally abstracted LAC
    */
  def maxAbstraction(spec: AST.Specification, config: Config): LinAlgCore = {
    val state = realPlusDefs(spec).map(id => (id -> realAdditionFromId(SymbolIdentifier(id, 0)))).toMap
    LinAlgCore(state, state, Map(), Map(), config)
  }

  private var varIdx: Int = -1

  /**
    * Retrieves a globally unused id.
    *
    * @return A fresh id
    */
  def newVarId: Int = {varIdx += 1; varIdx}

}

/**
  * A LinAlgCore (LAC) is used by [[Z3CombiConstraintState]]s to allow better handling of linear algebraic (LA) formulas.
  * Instead of a set of constraints that implicitly describe the values of streams by limiting the possible values cetain symblos can take,
  * here each stream maps explicitly to a [[RealAddition]], a LinAlg formula.
  * This allows especially usage of the pruning algorithm employed in [[Z3LinAlgOptimization]].
  *
  * @param state Maps each defined stream to an LA formula. Is used to calculate new LACs
  * @param stateWithFuture Same as state but under consideration of the future, that is, the LAC for the next step. Is only used to obtain values
  * @param inputs Maps each input stream to an LA formula
  * @param ranges Stores value ranges for uncertain streams as well as for temporary variables
  * @param config A configuration
  */
case class LinAlgCore(state: Map[Identifier, RealAddition], stateWithFuture: Map[Identifier, RealAddition], inputs: Map[Identifier, RealAddition], ranges: Map[SupIdentifier, OInterval], config: Config) {

  // Returns the set of variables a state depends on.
  private def dependentVarsInState(state: Map[Identifier, RealAddition]): Set[SupIdentifier] = {
    state.values.flatMap(_.getIds).toSet
  }

  // Returns the set of variables a state depends on that are irrelevant for the current time step, i.e. have timestamps outside of {-1,0,1}.
  private def irrelevantVarsInState(state: Map[Identifier, RealAddition]): Set[SupIdentifier] = {
    dependentVarsInState(state).filter {
      case SymbolIdentifier(_, ts) => !Set(-1,0,1).contains(ts)
      case _ => true
    }
  }

  // Returns a new LAC without ranges for variables that do not occurr in either state.
  private def removeUnusedRanges(): LinAlgCore = {
    val newRanges = ranges.filter {
      case (id, _) => (dependentVarsInState(state) ++ dependentVarsInState(stateWithFuture)).contains(id)
    }
    copy(ranges = newRanges)
  }

  // Replaces 0-symbols in an RA by other RAs.
  // This is used after time shifting and during forward or backward calculation to replace previously +1/-1-symbols with now available information.
  // getVal is a function used to calculate the new RA but since this parameter is always the function mkAndStoreRA(s)FromStream this also immediately stores the new RA.
  private def replaceZeroOffsets(ra: RealAddition, getVal: Identifier => RealAddition): RealAddition = {
    ra.coeff.foldLeft(RealAddition(Map(), ra.offset)) {
      case (curr, (id, d)) =>
        val nextRA = id match {
          case SymbolIdentifier(baseId, 0) => getVal(baseId)
          case _ => RealAddition(Map(id -> d), 0)
        }
        buildRealAdditionFromApp(AST.plus, curr, nextRA)
    }
  }

  // Checks whether a boolean AST.Expression is satisfiable under a constraint state.
  private def checkExp(exp: AST.Expression, ts: Int, maxTs: Option[Int], cs: Z3CombiConstraintState): Option[Boolean] = {
    cs.getQuery.boolQuery(Z3Utilities.mkZ3Expr(exp, ts, maxTs, cs.ctx).asInstanceOf[Expr[BoolSort]])
  }

  // Computes the value range a real AST.Expression describes under a constraint state.
  private def getRangeFromExpAndRA(exp: AST.Expression, ra: RealAddition, ts: Int, maxTs: Option[Int], cs: Z3CombiConstraintState): OInterval = {
    cs.getQuery.combinedBoundQuery(Z3Utilities.mkZ3Expr(exp, ts, maxTs, cs.ctx).asInstanceOf[Expr[RealSort]], ra)
  }

  // Retrieves the ranges that limit symbols with timestamp 0.
  private def currentRanges: Map[SupIdentifier, OInterval] = {
    ranges.filter {
      case (SymbolIdentifier(_, 0), _) => true
      case _ => false
    }
  }

  /**
    * Returns a new LAC with improved value ranges for uncertain inputs by querying each input range.
    * Should be done after construction of next LAC to make use of the information provided by the LAC.
    * (Because otherwise any query calls will use the the constraint state that still uses the old LAC.) 
    *
    * @param cs The current constraint state
    * @return The LAC with improved input ranges
    */
  def improveInputRanges(cs: Z3CombiConstraintState): LinAlgCore = {
    val newRanges = ranges ++ inputs.map {
      case (id, ra) => (SymbolIdentifier(id, 0) -> cs.getQuery.combinedBoundQuery(Z3Utilities.mkIdExpr(id, 0, cs.ctx).asInstanceOf[Expr[RealSort]], ra))
    }
    copy(ranges = newRanges)
  }

  /**
    * Applies the specification to obtain the LAC for the next time step.
    * In contrast to applySpecBwd, this computes different state and stateWithFuture.
    * The second is used to obtain values for streams, while the first is used to generate the next LAC.
    * The stateWithFuture contains approximations for the next step but since the next LAC 
    * itself is more precise due to now available inputs, the future of the current LAC (i.e. the present of the next) is of no advantage there.
    *
    * @param spec The specification
    * @param ts The current timestamp
    * @param cs The corresponding constraint state
    * @param qcc The corresponding QuickCalcCore
    * @return The LAC for the next time step
    */
  def applySpecFwd(spec: AST.Specification, ts: Int, cs: Z3CombiConstraintState, qcc: QuickCalcCore): LinAlgCore = {
    val future = cs.future.map(_.timeShift(-1))
    var newState = inputs
    var newStateWithFuture = inputs
    var newRanges = currentRanges // keep ranges from inputs

    // Computes RAs for a stream.
    def mkAndStoreRAsFromStream(id: Identifier): (RealAddition, RealAddition) = {
      if (!newState.contains(id)) {
        val (newStateVal, newFutureStateVal) = {
          if (qcc.get(id).isDefined) {
            val d = realAdditionFromValue(qcc.get(id).get.asInstanceOf[RealValue].v)
            (d, d)
          } else {
            mkRAsFromExp(spec.definitions(id))
          }
        }
        newState += (id -> newStateVal)
        newStateWithFuture += (id -> newFutureStateVal)
      }
      (newState(id), newStateWithFuture(id))
    }

    // Computes RAs for an expression.
    def mkRAsFromExp(exp: AST.Expression): (RealAddition, RealAddition) = {
      // Applies getRangeFromExpAndRA on multiple RAs at once.
      def getRangesFromExpAndRAs(exp: AST.Expression, ras: Seq[RealAddition]): Seq[OInterval] = {
        ras.map(getRangeFromExpAndRA(exp, _, ts, config.maxTs, cs))
      }

      exp match {
        case AST.Constant(v) => 
          val ra = realAdditionFromValue(v.asInstanceOf[RealValue].v)
          (ra, ra)
        case id: AST.Identifier => mkAndStoreRAsFromStream(id)
        case AST.Offset(id, 0, _) => mkRAsFromExp(id)
        case AST.Offset(id, -1, default) =>
          val ra = state.getOrElse(id, mkRAsFromExp(AST.Constant(default))._1)
          newRanges ++= ranges.filter{case (id, _) => ra.getIds.contains(id)} // import ranges from copied RA
          (replaceZeroOffsets(ra, (id: Identifier) => mkAndStoreRAsFromStream(id)._1), 
          replaceZeroOffsets(ra, (id: Identifier) => mkAndStoreRAsFromStream(id)._2)) // replace previous +1-symbols by now available RAs (time shift done before)
        case AST.Offset(_, 1, default) if config.maxTs.contains(ts) => mkRAsFromExp(AST.Constant(default))
        case AST.Offset(id, 1, _) if future.isEmpty => // future input unavailable
          val ra = realAdditionFromId(SymbolIdentifier(id, 1))
          (ra, ra)
        case AST.Offset(id, 1, _) => // only case where state and stateWithFuture differ
          val ra = realAdditionFromId(SymbolIdentifier(id, 1))
          val futureLac = future.get.linAlgCore
          newRanges ++ futureLac.ranges.filter{case (id, _) => ra.getIds.contains(id)} // import ranges from future
          //   replace previous -1-symbols by now available RAs (time shift done before)
          (ra, replaceZeroOffsets(futureLac.state(id), (id: Identifier) => mkAndStoreRAsFromStream(id)._2))
        case AST.Application(AST.ite, exps) => 
          checkExp(exps.head, ts, config.maxTs, cs) match {
            case Some(true) => mkRAsFromExp(exps(1))
            case Some(false) => mkRAsFromExp(exps(2))
            case None => // ite condition uncertain? -> new variable with range from intersection of both branches
              val trueRAs = mkRAsFromExp(exps(1))
              val falseRAs = mkRAsFromExp(exps(2))
              val rids = (
                Identifier(s"IF#${LinAlgCore.newVarId}", Real), 
                Identifier(s"IFfut#${LinAlgCore.newVarId}", Real))
              val trueRanges = getRangesFromExpAndRAs(exps(1), Seq(trueRAs._1, trueRAs._2))
              val falseRanges = getRangesFromExpAndRAs(exps(2), Seq(falseRAs._1, falseRAs._2))
              newRanges += (rids._1 -> Common.unionIntervals(trueRanges(0), falseRanges(0)))
              newRanges += (rids._2 -> Common.unionIntervals(trueRanges(1), falseRanges(1)))
              (realAdditionFromId(rids._1), realAdditionFromId(rids._2))
          }
        case AST.Application(op, exps) =>
          val mappedExps = exps.map(mkRAsFromExp)
          (buildRealAdditionFromApp(op, mappedExps.map(_._1)),
          buildRealAdditionFromApp(op, mappedExps.map(_._2)))
      }
    }

    LinAlgCore.realDefs(spec).foreach{case (id, _) => mkAndStoreRAsFromStream(id)}

    copy(state = newState, stateWithFuture = newStateWithFuture, ranges = newRanges).removeUnusedRanges()
  }

  /**
    * Applies the specification to obtain the LAC for the previous time step.
    * Since this is only called during the future estimation and, hence, the previously computed state is the future state itself, 
    * state and stateWithFuture will always be the same.
    *
    * @param spec The specification
    * @param ts The current timestamp
    * @param cs The corresponding constraint state
    * @return The LAC for the previous time step
    */
  def applySpecBwd(spec: AST.Specification, ts: Int, cs: Z3CombiConstraintState): LinAlgCore = {
    var newState = spec.inputs.foldLeft(Map[Identifier, RealAddition]()) {
      case (curr, (s, Real)) =>
        val id = Identifier(s, Real)
        curr + (id -> realAdditionFromId(SymbolIdentifier(id, 0)))
      case (curr, _) => curr
    }
    var newRanges = Map[SupIdentifier, OInterval]()
    //var newRanges = spec.inputs.foldLeft(Map[SupIdentifier, OInterval]()) {
    //  case (curr, (s, Real)) => curr + (SymbolIdentifier(Identifier(s, Real), 0) -> (None, None))
    //  case (curr, _) => curr
    //}

    // Computes RA for a stream.
    def mkAndStoreRAFromStream(id: Identifier): RealAddition = {
      if (!newState.contains(id)) {
        newState += (id -> mkRAFromExp(spec.definitions(id)))
      }
      newState(id)
    }

    // Computes RA for an expression.
    def mkRAFromExp(exp: AST.Expression): RealAddition = {
      exp match {
        case AST.Constant(v) => realAdditionFromValue(v.asInstanceOf[RealValue].v)
        case id: AST.Identifier => mkAndStoreRAFromStream(id)
        case AST.Offset(id, 0, _) => mkRAFromExp(id)
        case AST.Offset(id, -1, _) => realAdditionFromId(SymbolIdentifier(id, -1)) // past not available during backwards calculation
        case AST.Offset(id, 1, default) => 
          val ra = state.getOrElse(id, mkRAFromExp(AST.Constant(default)))
          newRanges ++= ranges.filter{case (id, _) => ra.getIds.contains(id)}
          replaceZeroOffsets(ra, (id: Identifier) => mkAndStoreRAFromStream(id))
        case AST.Application(AST.ite, exps) => 
          checkExp(exps.head, ts, config.maxTs, cs) match {
            case Some(true) => mkRAFromExp(exps(1))
            case Some(false) => mkRAFromExp(exps(2))
            case None => // ite condition uncertain? -> new variable with range from intersection of both branches
              val trueRA = mkRAFromExp(exps(1))
              val falseRA = mkRAFromExp(exps(2))
              val rid = Identifier(s"IF#${LinAlgCore.newVarId}", Real)
              val trueRange = getRangeFromExpAndRA(exps(1), trueRA, ts, config.maxTs, cs)
              val falseRange = getRangeFromExpAndRA(exps(2), falseRA, ts, config.maxTs, cs)
              newRanges += (rid -> Common.unionIntervals(trueRange, falseRange))
              realAdditionFromId(rid)
          }
        case AST.Application(op, exps) => buildRealAdditionFromApp(op, exps.map(mkRAFromExp))
      }
    }

    // Generate RAs for every positively referenced defined stream, others are not neccessary since this LAC is only used as future
    LinAlgCore.realPlusDefs(spec).foreach(mkAndStoreRAFromStream)

    copy(state = newState, stateWithFuture = newState, ranges = newRanges).removeUnusedRanges()
  }

  /**
    * Rewrites the LAC using the pruning algorithm in [[Z3LinAlgOptimization]] to reduce the LAC's size.
    *
    * @param spare The set of variables to spare
    * @param cs The current constraint state
    * @return The rewritten LAC
    */
  def rewrite(spare: Set[SupIdentifier], cs: Z3CombiConstraintState): LinAlgCore = {
    var newRanges: Map[SupIdentifier, OInterval] = ranges

    // Rewrites a single state.
    def rewriteState(state: Map[Identifier, RealAddition]): Map[Identifier, RealAddition] = {
      // Simplfy groups are sets (here seqs to keep the order) of variables that are dependent on each other.
      // Variables in two different simplify groups are independent of each other.
      def determineSimplifyGroups(): Set[Seq[Identifier]] = {
        val uf = new UnionFind[Identifier]
        state.keys.foreach(uf.find)

        state.foreach {
          case (id1, def1) =>
            val vg1 = def1.getIds
            state.foreach {
              case (id2, def2) =>
                if (id1 != id2 && id1.typ == id2.typ && vg1.intersect(def2.getIds).nonEmpty) {
                    uf.union(id1, id2)
                }
            }
        }

        uf.toSeqMap.values.toSet
      }

      determineSimplifyGroups().flatMap {
        case id :: _ if state(id).isConst => Seq((id, state(id))) // if head is const, then this group is a singleton
        case g =>
          val sGroup = g.sortBy(id => state(id).getIds.size)
          val ras = sGroup.map(state(_))
          val (simplified, simpRanges) = Z3LinAlgOptimization.simplify(ras, cs, spare)
          newRanges ++= simpRanges // new ranges may be introduced during this
          sGroup.zip(simplified)
      }.toMap
    }

    val newSt = rewriteState(state)
    val newFSt = rewriteState(stateWithFuture)
    val usedIds = dependentVarsInState(newSt) ++ dependentVarsInState(newFSt) // used to cleanup ranges

    copy(state = newSt, stateWithFuture = newFSt, ranges = newRanges.filter(e => usedIds.contains(e._1)))
  }

  /**
    * Widen the previous LAC by this LAC.
    *
    * @param currCs Current constraint state
    * @param prevCs Previous constraint state
    * @return
    */
  def widen(prevCs: Z3CombiConstraintState, currCs: Z3CombiConstraintState): LinAlgCore = {
    var newRanges : Map[SupIdentifier, OInterval] = Map()

    // Widens state s1 by state s2.
    def widenStates(s1: Map[Identifier, RealAddition], s2: Map[Identifier, RealAddition]): Map[Identifier, RealAddition] = {
      s1.keys.foldLeft[Map[Identifier, RealAddition]](Map()) {
        case (newState, id) =>

          val currCond = currCs.spec.linkedStreams.get(id).map(id => currCs.ctx.mkBoolConst(SymbolIdentifier(id, 0).toString))
          val prevCond = prevCs.spec.linkedStreams.get(id).map(id => prevCs.ctx.mkBoolConst(SymbolIdentifier(id, 0).toString))

          val prevSid = Z3Utilities.mkIdExpr(id, 0, prevCs.ctx).asInstanceOf[Expr[RealSort]]
          val currSid = Z3Utilities.mkIdExpr(id, 0, currCs.ctx).asInstanceOf[Expr[RealSort]]
          val i1 = prevCs.getQuery.combinedBoundQuery(prevSid, s1(id), prevCond)
          val i2 = currCs.getQuery.combinedBoundQuery(currSid, s2(id), currCond)

          val (left, right) = Common.widenIntervals(i1, i2)

          if (left.isEmpty || right.isEmpty || left.get != right.get) {
            val nid = Identifier(s"WIDEN#${LinAlgCore.newVarId}", Real)
            newRanges += (nid -> (left, right))
            newState + (id -> realAdditionFromId(nid))
          } else {
            newState + (id -> realAdditionFromValue(left.get))
          }
      }
    }

    val newState = widenStates(prevCs.linAlgCore.state, state)
    val newStateWithFuture = if (state == stateWithFuture && prevCs.linAlgCore.state == prevCs.linAlgCore.stateWithFuture) {
      newState // if stateWithFuture for this LAC and the previous LAC are identical, the widening states will also be identical
    } else {
      widenStates(prevCs.linAlgCore.stateWithFuture, stateWithFuture)
    }

    copy(state = newState, stateWithFuture = newStateWithFuture, ranges = newRanges)
  }

  /**
    * Gets the LA formula of a defined stream as [[RealAddition]]. 
    *
    * @param id The id of the desired stream
    * @return The value of id
    */
  def getIDValue(id: Identifier): RealAddition = {
    stateWithFuture(id)
  }

  // Builds z3 expression from a state by combining state and ranges
  private def mkZ3ExpFromState(state: Map[Identifier, RealAddition]): Expr[BoolSort] = {
    val ctx = config.ctx
    val rPart = ranges.filter{case (id, int) => dependentVarsInState(state).contains(id)}.flatMap{case (id, int) => mkRangeZ3Expressions(id, int, ctx)}
    val sPart = state.map{case (id, ra) => ctx.mkEq(Z3Utilities.mkIdExpr(id, 0, ctx), ra.toZ3Exp(ctx))}
    ctx.mkAnd((rPart ++ sPart).toSeq: _*)
  }

  /**
    * Returns z3 representation of the LAC including the future.
    * Symbols with timestamps outside of {-1,0,1} are eliminated beforehand.
    *
    * @param addKill Additional variables that shall be eliminated in the expression
    * @return The z3 expression
    */
  def getZ3Representation(addKill : Set[SupIdentifier] = Set()): Expr[BoolSort] = {
    val kill = addKill ++ irrelevantVarsInState(stateWithFuture) 
    Z3Utilities.quantifierElimination(mkZ3ExpFromState(stateWithFuture), kill, config)
  }

  /**
    * Returns z3 representation of the LAC excluding the future.
    * Symbols with timestamps outside of {-1,0,1} are eliminated beforehand.
    *
    * @param addKill Additional variables that shall be eliminated in the expression
    * @return The z3 expression
    */
  def getZ3RepresentationState(addKill: Set[SupIdentifier] = Set()): Expr[BoolSort] = {
    val kill = addKill ++ irrelevantVarsInState(state)
    Z3Utilities.quantifierElimination(mkZ3ExpFromState(state), kill, config)
  }

  /**
    * Checks whether this LAC is congruent to another LAC, i.e. their z3 representatives are identical.
    * ignore should be the set of variables that were previously erased during simplification or cleanup.
    * Since the LAC does not change during cleanup, this ignores them at least during the consistency check.
    *
    * @param other The other LAC
    * @param ignore Symbols that will be eliminated before congruency check and thus be ignored
    * @return True if congruent, false if not
    */
  def isCongruentTo(other: LinAlgCore, ignore: Set[SupIdentifier] = Set()): Boolean = {
    if (state == stateWithFuture && other.state == other.stateWithFuture) {
      Z3Utilities.checkEquivalence(getZ3RepresentationState(ignore), other.getZ3RepresentationState(ignore), config)
    } else {
      Z3Utilities.checkEquivalence(getZ3Representation(ignore), other.getZ3Representation(ignore), config) &&
      Z3Utilities.checkEquivalence(getZ3RepresentationState(ignore), other.getZ3RepresentationState(ignore), config)
    }
  }

  /**
    * Adds an input value for a an input stream.
    * This adds a corresponding RA to the input field and possibly a new range.
    *
    * @param id The identifier of the input stream
    * @param value The input value
    * @return The LAC including the input
    */
  def addInput(id: Identifier, value: Value): LinAlgCore = {
    val (newInp, newR) = value match {
      case RealValue(v) =>
        val ne = (id -> realAdditionFromValue(v))
        (inputs + ne, ranges)
      case RangeValue(from, to, _) =>
        val sid = SymbolIdentifier(id, 0)
        val ne = (id -> realAdditionFromId(sid))
        (inputs + ne, ranges + (sid -> (from, to)))
      case setValue: RealSetValue => 
        val sid = SymbolIdentifier(id, 0)
        val ne = (id -> realAdditionFromId(sid))
        val rangeValue = setValue.toRangeValue
        (inputs + ne, ranges + (sid -> (rangeValue.from, rangeValue.to))) //TODO can we use RealSets without abstracting to ranges?
      case _ => (inputs, ranges)
    }

    copy(inputs = newInp, ranges = newR)
  }

  /**
    * Shifts the LAC by a number of timesteps, i.e. shifting every symbol s^t to s^t-o.
    *
    * @param o The offsets determining the new timestamp
    * @return The shifted LAC
    */
  def timeShift(o: Int): LinAlgCore = {
    // Adjusts the timestamp of a single symbol.
    def adjId(oid: SymbolIdentifier): SymbolIdentifier = {
      oid.copy(ts = oid.ts - o)
    }

    // Adjust all symbols in a single RealAddition.
    def iterateRA(v: RealAddition): RealAddition = {
      val f = v.getIds.toSeq.filter(_.isInstanceOf[SymbolIdentifier])
      val t = f.map(i => adjId(i.asInstanceOf[SymbolIdentifier]))
      v.replaceIDs(f, t)
    }

    val newSt = state.map{case (id, ra) => (id, iterateRA(ra))}
    val newFuSt = stateWithFuture.map{case (id, ra) => (id, iterateRA(ra))}
    val newInp = inputs.map{case (id, ra) => (id, iterateRA(ra))}
    val newR = ranges.map{case (id, v) => id match {
      case id: SymbolIdentifier => (adjId(id), v)
      case _ => (id, v)
    }}

    copy(inputs = newInp, state = newSt, stateWithFuture = newFuSt, ranges = newR)
  }

  /** @inheritdoc */
  override def toString: String = {
    s"""
      |$state
      |--
      |$stateWithFuture
      |--
      |$inputs
      |--
      |$ranges
      |""".stripMargin
  }

}
