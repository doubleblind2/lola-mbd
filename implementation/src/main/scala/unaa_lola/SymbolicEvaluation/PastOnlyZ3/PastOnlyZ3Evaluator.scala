package unaa_lola.SymbolicEvaluation.PastOnlyZ3

import unaa_lola.AST
import unaa_lola.AST._
import unaa_lola.Value.{Value => NewValue, BoolValue => NewBoolValue, RealValue => NewRealValue, UnknownBoolValue => NewUnknownBoolValue, RangeValue => NewRangeValue}
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.LinAlg.{Z3LinAlgFormula, Z3LinAlgOptimization}
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.Logical.Z3LogicalOptimization
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.POValue.{Value, BoolVal, RealVal, UncertainRealValue, UncertainBoolValue}
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.{Z3QueryEnvironment, Z3ValueFactory}
import unaa_lola.Evaluator
import unaa_lola.Value.{Type, Real, Bool}
import unaa_lola.util.{Errors, Logging, UnionFind}
import unaa_lola.util.Common.{OInterval, assStream}
import unaa_lola.SymbolicEvaluation.z3Engine.LinAlgCore
import unaa_lola.CertainEvaluation.CertainEvaluator
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.PastOnlyZ3Evaluator.{poValToNewVal, newValToPoVal, ReplacementIdentifier}
import unaa_lola.SymbolicEvaluation.SimplificationStrategy
import unaa_lola.Value
import unaa_lola.Config

object PastOnlyZ3Evaluator {

  case class ReplacementIdentifier(override val name: String, override val typ: Type, op: Operator, vals: Seq[Value]) extends SupIdentifier(name, typ) {
    override def toString: String = s"$name"
    //override def toString: String = s"""$name-$op(${vals.mkString(",")})"""
  }

  // Workaround to get past only evaluator to work with new architecture
  case class CarrierValue(v: Value) extends NewValue {
    override def getType: Type = v.getType
    override def toString(verbose: Boolean): String = v.toString
    override def isCertain: Boolean = v.isCertain
  }

  def poValToNewVal(value: Value): NewValue = {
    value match {
      case BoolVal(v) => NewBoolValue(v)
      case RealVal(v) => NewRealValue(v)
      case UncertainBoolValue(v) => NewUnknownBoolValue(desc = v.toString)
      case UncertainRealValue(v) => 
        val range = v.getValueRepresentation
        NewRangeValue(range._1, range._2, desc = v.toString)
    }
  }

  def newValToPoVal(value: NewValue): Value = {
    value match {
      case NewBoolValue(v) => BoolVal(v)
      case NewRealValue(v) => RealVal(v)
    }
  }

}

/**
  * Legacy evaluator from old tool version. Relies much less on Z3. 
  * Rather than keeping a single big Z3 expression and performing operations on that, a map mapping each state to its own internal expression is kept.
  * Only uses Z3 for producing output, everything else is done manually.
  * This is much less flexible and makes anticipation basically impractial but is often faster if only negative offsets occur.
  * 
  * TODO Either remove this or integrate this into the existing framework as much as possible. Reuse RealAdditions instead of Z3LinAlgFormulas, use the existing optimization class, factories, values etc. 
  * Be aware that the new values only contain the actual values while the old ones also contained formulaic representations.
  * TODO This was implemented before enums, real sets, sampling and linked streams were conceived, i.e., they do not work.
  */
class PastOnlyZ3Evaluator(spec: ExactSpecification, config: Config) extends Evaluator(spec, config, false) {

  private val optimizeInterval = 1
  private val env: QueryEnvironment = new Z3QueryEnvironment(config)
  private val fact: ValueFactory = new Z3ValueFactory(env.asInstanceOf[Z3QueryEnvironment])
  private val logicalSimplifier: FormulaOptimization[Formula[Option[Boolean]]] = Z3LogicalOptimization
  private val linAlgSimplifier: FormulaOptimization[Formula[OInterval]]= Z3LinAlgOptimization

  Z3LinAlgOptimization.loadFramework

  val realStateVars: Set[Identifier] = buildRealStateVars()
  var simplifyVars: Set[Identifier] = Set()

  var lastState: Map[Identifier, Value] = Map()
  var state: Map[Identifier, Value] = Map()

  /** @inheritdoc */
  def eval(): Unit = {
    val assumptionExists = flatspec.definitions.contains(assStream)

    lastState = state
    state = Map()
    simplifyVars = realStateVars

    consumeInputs.foreach{case (stream, value) => readInputVal(stream, value)}

    val from = System.nanoTime();

    if (assumptionExists) {
      val stateBeforeAss = state
      evalStream(assStream, Set())
      env.addAssumption(state.get(assStream))
      env.resetCaches()

      state(assStream) match {
        case BoolVal(false) => throw new Errors.InternalException("Assumption is broken!")
        case _ =>
      }

      state = stateBeforeAss
    }

    flatspec.outputs.foreach{
      s =>
        evalStream(s, Set())
        singleValOutput(s, poValToNewVal(state(s)))
    }

    realStateVars.foreach{
      s => evalStream(s, Set())
    }

    val to1 = System.nanoTime();

    //if ((ts % optimizeInterval) == 0) {
    if (config.simplificationStrategy.optimizationNecessary) {
      simplify()
      env.clearAssumptions()
    }
    config.simplificationStrategy.proceedStep()

    env.resetCaches()

    if (ts % 20 == 0) System.gc()

    val to2 = System.nanoTime();

    Logging.log(s"Simplification took ${(to2 - to1) / 1000} μs, Evaluation took ${(to1 - from) / 1000} μs" + (if (config.collectZ3Runtimes) s", z3 calls took ${config.z3RuntimeCollector.getTotalTimeInMicro} µs" else ""))
    Logging.log(s"${(to2 - to1) / 1000},${(to1 - from) / 1000}" + (if (config.collectZ3Runtimes) s",${config.z3RuntimeCollector.getTotalTimeInMicro}" else ""), channel = Logging.RUNTIMES)
    config.z3RuntimeCollector.reset()
  }

  def readInputVal(stream: Identifier, value: NewValue): Unit = {
    val nid = Identifier(SymbolIdentifier(stream, ts).name, stream.getType)

    val input = value match {
      case NewBoolValue(_) | NewRealValue(_) => newValToPoVal(value)
      case NewUnknownBoolValue(_) => fact.buildConstant(nid)
      case NewRangeValue(from, to, _) =>
        env.addRange(nid, from, to)
        fact.buildConstant(nid)
      case v: Value.RealSetValue => return readInputVal(stream, v.toRangeValue)
    }

    state += (stream -> input)
  }

  private def evalStream(id: Identifier, stack: Set[Identifier]): Option[Value] = {
    if (!state.contains(id) && !stack.contains(id)) {
      val res = evalExpr(flatspec.definitions(id), stack + id)
      val sres = if (realStateVars.contains(id) || flatspec.outputs.contains(id)) res.materialize else res

        sres match {
        case RealVal(_) | BoolVal(_) =>
          simplifyVars -= id
        case _ =>
      }
      state += (id -> sres)
    }
    state.get(id)
  }

  private def evalExpr(exp: Expression, stack: Set[Identifier]): Value = {
    exp match {
      case Application(op, exps) => applyOp(op, exps.map(evalExpr(_, stack)))
      case Offset(exp, offset, default) if offset == -1 && exp.isInstanceOf[Identifier] =>
        val id = exp.asInstanceOf[Identifier]
        evalStream(id, stack)
        lastState.getOrElse(id, newValToPoVal(default))
      case Offset(exp, offset, _) if offset == 0 => evalExpr(exp, stack)
      case _: Offset => throw new Errors.InternalException(s"Positive offsets not supported in this evaluator!")
      case Constant(v) => newValToPoVal(v)
      case i: Identifier => evalStream(i, stack).getOrElse{throw new Errors.InternalException(s"evalStream($i, $stack) delivered no result!")}
    }
  }

    /**
   * Solve application of operator on sequence df values
   * @param op Operator to be applied
   * @param vals Values on which operation is applied
   * @return Resulting value (may be certain or uncertain)
   */
  private def solve(op: Operator, vals: Seq[Value]): Value = {
    op match {
      case AST.ite if vals(1).getType == Real =>
        val cond = vals.head

        if (cond.isCertain) {
          if (cond.asInstanceOf[BoolVal].value) vals(1) else vals(2)
        } else {
          Logging.log(s"Uncertain condition in if statement occurred: $cond")
          val in = union(vals(1), vals(2))

          if (in._1.isEmpty || in._2.isEmpty || in._1.get != in._2.get) {
            val nid = Identifier(s"IF#${LinAlgCore.newVarId}", Real)
            env.addRange(nid, in._1, in._2)
            env.addIftoAssumption(nid, cond, vals(1), vals(2))
            fact.buildConstant(nid)
          } else {
            RealVal(in._1.get)
          }
        }
      case _ => fact.buildFromASTOp(op, vals)
    }
  }

  private def applyOp(op: Operator, vals: Seq[Value]): Value = {
    if (vals.forall(_.isCertain)) {
      newValToPoVal(CertainEvaluator.applyOp(op, vals.map(poValToNewVal)))
    } else {
      solve(op, vals)
    }
  }

  // was called widen before
  private def union(v1: Value, v2: Value): (Option[Double], Option[Double]) = {

    def getInt(v: Value): (Option[Double], Option[Double]) = {
      v match {
        case RealVal(value) => (Some(value), Some(value))
        case UncertainRealValue(value) => value.getValueRepresentation
      }
    }

    val i1 = getInt(v1)
    val i2 = getInt(v2)

    (
      if (i1._1.isEmpty) i2._1 else if (i2._1.isEmpty) i1._1 else Some(Math.min(i1._1.get, i2._1.get)),
      if (i1._2.isEmpty) i2._2 else if (i2._2.isEmpty) i1._2 else Some(Math.max(i1._2.get, i2._2.get)),
    )
  }

  /**
   * Simplify subsystems of equations to prune variables.
   * Called after every optimizeInterval steps
   */
  private def simplify(): Unit = {
    val simplifyGroups: Set[Seq[Identifier]] = determineSimplifyGroups()

    simplifyGroups.foreach{
      case g if g.head.typ == Bool =>
        val sGroup = g.filter(e => state.contains(e) && !state(e).isCertain)
        val simplified = logicalSimplifier.simplify(sGroup.map(state(_).asInstanceOf[UncertainBoolValue].formula))
        sGroup.zip(simplified).foreach{case (i, v) => state += (i -> v.toValue)}
      case g if g.head.typ == Real =>
        val sGroup = g.filter(e => state.contains(e) && !state(e).isCertain).sortBy(e => state(e).depVarNum)
        val simplified = linAlgSimplifier.simplify(sGroup.map(state(_).asInstanceOf[UncertainRealValue].formula))
        sGroup.zip(simplified).foreach { case (i, v) => state += (i -> v.toValue) }
    }


    def getRealVars(v: Value): Set[Identifier] = {
      v match {
        case UncertainRealValue(value) => value.dependendVariables.asInstanceOf[Set[Identifier]]
        case UncertainBoolValue(value) => value.dependendVariables.flatMap{
            case ReplacementIdentifier(_, _, _, vals) => vals.flatMap(getRealVars)
            case _ => None
          }
        case _ => Set()
      }
    }

    val allUnknownReals = state.foldLeft[Set[Identifier]](Set()){
      case (c, (_, v)) => c ++ getRealVars(v)
    }

    env.clearRangesExceptFrom(allUnknownReals)
  }

  private def determineSimplifyGroups(): Set[Seq[Identifier]] = {
    val uf = new UnionFind[Identifier]

    simplifyVars.foreach{
      id => uf.find(id)
    }

    simplifyVars.foreach{
      id1 =>
        val vg1 = getDefiningRTVars(id1)
        simplifyVars.foreach{
          id2 =>
            if (id1 != id2 && id1.typ == id2.typ && vg1.intersect(getDefiningRTVars(id2)).nonEmpty) {
                uf.union(id1, id2)
            }
        }
    }

    uf.toSeqMap.values.toSet
  }

  private def getDefiningRTVars(id: Identifier): Set[Identifier] = {

    def crawlValues(v: Value): Set[Identifier] = {
      v match {
        case UncertainRealValue(value) => value.dependendVariables.asInstanceOf[Set[Identifier]]
        case UncertainBoolValue(_) => throw new Errors.InternalException("Bool identifier inside Replacement identifier currently not supported!")
        case _ => Set()
      }
    }

    if (state.contains(id)) {
      state(id) match {
        case UncertainRealValue(value) => value.dependendVariables.asInstanceOf[Set[Identifier]]
        case UncertainBoolValue(value) => value.dependendVariables.foldLeft[Set[Identifier]](Set()) {
          case (c, i: Identifier) => c + i
          case (c, ReplacementIdentifier(_, _, _, vals)) => c ++ vals.flatMap(crawlValues)
        }
        case _ => Set()
      }
    } else {
      Set()
    }
  }

  private def buildRealStateVars() : Set[Identifier] = {

    var realStateVars: Set[Identifier] = Set()

    def queryOffsets(e: Expression) : Set[Identifier] = {
      e match {
        case Application(_, exps) => exps.foldLeft[Set[Identifier]](Set()){case (c,e) => c ++ queryOffsets(e)}
        case Offset(id: Identifier, offset, _) if offset != 0 => Set(id)
        case Offset(exp, _, _) => queryOffsets(exp)
        case _ => Set()
      }
    }

    flatspec.definitions.foreach{
      case (_, e) =>
        realStateVars = realStateVars ++ queryOffsets(e)
    }

    realStateVars
  }

}