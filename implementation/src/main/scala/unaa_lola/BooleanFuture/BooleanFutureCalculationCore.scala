package unaa_lola.BooleanFuture

import unaa_lola.AST.Specification

import unaa_lola.AST
import unaa_lola.AST.{Expression, Identifier}
import unaa_lola.Value.{Real, Bool, BoolValue}
import unaa_lola.BooleanFuture.BooleanFutureEvaluator.State
import unaa_lola.CertainEvaluation.CertainEvaluator
import unaa_lola.util.Errors

import scala.collection.BitSet

/**
 * This class contains core functions for the speculative calculation of LOLA specifications with future
 * @param spec The LOLA specification to compute the future calculations on
 */
class BooleanFutureCalculationCore(spec: Specification) {

  private val assumption = Identifier("ASSUMPTION", Bool)

  val inputsSeq: Seq[Identifier] = spec.inputs.keys.toSeq.map(i => Identifier(i, Bool))
  val stateVariablesFuture: Seq[Identifier] = spec.definitions.foldLeft[Set[Identifier]](Set()) { case (c, (_, e)) => c ++ queryOffset(e, +1) }.toSeq
  val stateVariablesPast: Seq[Identifier] = spec.definitions.foldLeft[Set[Identifier]](Set()) { case (c, (_, e)) => c ++ queryOffset(e, -1) }.toSeq
  val stateVariables: Seq[Identifier] = stateVariablesPast ++ inputsSeq

  /**
   * Pretty format a LOLA State with future guessings
   * @param s The state to be formatted
   * @return Pretty string representation of the state
   */
  def pp(s: State): String = s.prettyPrint(stateVariables, stateVariablesFuture)

  private def queryOffset(e: Expression, reqOffset: Int): Set[Identifier] = {
    e match {
      case AST.Application(_, exps) => exps.foldLeft[Set[Identifier]](Set()) { case (c, e) => c ++ queryOffset(e, reqOffset) }
      case AST.Offset(id, offset, _) if offset * offset > 1 || !id.isInstanceOf[AST.SupIdentifier] =>
        throw new Errors.InternalException("Non flat spec in BooleanFuture.EmptynessCheck", panic = true)
      case AST.Offset(id: AST.Identifier, offset, _) if offset == reqOffset => Set(id)
      case _ => Set()
    }
  }

  val futureGuessings: Set[BitSet] = {

    def getFullyDeterminedSet(i: Identifier) : Option[Set[Identifier]] = {
      def crawlExp(e: Expression): Option[Set[Identifier]] = {
        e match {
          case AST.Application(_, exps) => exps.foldLeft[Option[Set[Identifier]]](Some(Set())){
            (c, e) => c.flatMap(s1 => crawlExp(e).flatMap(s2 => Some(s1.union(s2))))
          }
          case AST.Offset(id, 0, _) => Some(Set(id))
          case AST.Offset(_, _, _) => None
          case AST.Constant(_) => Some(Set())
          case id: AST.SupIdentifier => Some(Set(id.asInstanceOf[Identifier]))
        }
      }

      if (spec.definitions.contains(i)) {
        crawlExp(spec.definitions(i))
      } else {
        None
      }
    }

    val essentials = stateVariablesFuture.filter{id =>
      val det = getFullyDeterminedSet(id)
      det.isEmpty || !det.get.subsetOf(stateVariablesFuture.toSet)
    }

    val combs = iterateFutureCombinations(essentials.length)

    def valueDetermine(e: Expression, ess: BitSet): Boolean = {
      e match {
        case AST.Application(op, exps) => {
          val rexps = exps.map(e => BoolValue(valueDetermine(e, ess)))
          CertainEvaluator.applyOp(op, rexps).asInstanceOf[BoolValue].v
        }
        case AST.Offset(id, 0, _) => valueDetermine(id, ess)
        case AST.Constant(v) => v.asInstanceOf[BoolValue].v
        case id: AST.SupIdentifier if essentials.contains(id) => ess(essentials.indexOf(id) + 1)
        case id: AST.SupIdentifier if !spec.inputs.contains(id.name) => valueDetermine(spec.definitions(id.asInstanceOf[Identifier]), ess)
        case _ => ??? //May not happen
      }
    }

    combs.map{
      ess =>
      stateVariablesFuture.foldLeft[BitSet](BitSet()){
        (c,i) =>
          if (essentials.contains(i)) {
            if (ess(essentials.indexOf(i) + 1)) {
              c + (stateVariablesFuture.indexOf(i) + 1)
            } else {
              c
            }
          } else {
            if (valueDetermine(i, ess)) {
              c + (stateVariablesFuture.indexOf(i) + 1)
            } else {
              c
            }
          }
      }
    }
  }

  val inputCombs: Set[BitSet] = iterateInputCombinations(inputsSeq.length)

  private def iterateInputCombinations(n: Int): Set[BitSet] = {
    if (n == 0) {
      Set(BitSet())
    } else {
      val sub = iterateInputCombinations(n - 1)
      sub ++ sub.map(bm => bm + (stateVariables.indexOf(inputsSeq(n - 1)) + 1))
    }
  }

  private def iterateFutureCombinations(n: Int): Set[BitSet] = {
    if (n == 0) {
      Set(BitSet())
    } else {
      val sub = iterateFutureCombinations(n - 1)
      sub ++ sub.map(bm => bm + n)
    }
  }

  /**
   * Calculate a set of follow-up states
   * @param curr The state for which follow up states shall be calculated
   * @param guess The guessing of future variables to be integrated
   * @param cachedState A cache state to accelerate calculation
   * @param beginning  If state curr is a state at pos t=0
   * @param includeTerm Include terminal states (with empty second component) in the result
   * @return The set of follow-up-states
   */
  def followUpStates(curr: State, guess: BitSet, cachedState: Map[Identifier, Boolean], beginning: Boolean = false, includeTerm: Boolean): Set[State] = {

    var newState = cachedState

    val calc = stateVariablesPast.foldLeft[BitSet](BitSet()) {
      (c, sv) => {
        val ev = eval(sv, curr, newState, beginning)
        newState = ev._2
        if (ev._1) c + (stateVariablesPast.indexOf(sv) + 1) else c
      }
    }

    val r = inputCombs.map {
      ic => State(ic | calc, Some(guess))
    }.union(
     if (includeTerm) {
      inputCombs.map {
        ic => State(ic | calc, None)
      }
     } else {
      Set()
    }).flatMap{
      s => if (assumptionCheck(s, Map(), beginning, s.next.isEmpty)._1) Some(s) else None
    }

    r

  }

  /**
   * Check if the transition between two states is valid
   * @param curr The state to which the transition goes
   * @param prev The state from which the transition comes
   * @param cachedState A cache state to accelerate calculation (refers to results of curr)
   * @return Tuple of outcome (if transition is valid) and new cache state for curr
   */
  def checkStatesCompatible(curr: State, prev: State, cachedState: Map[Identifier, Boolean]): (Boolean, Map[Identifier, Boolean]) = {
    var newState: Map[Identifier, Boolean] = cachedState
    stateVariablesFuture.foreach {
      id =>
        val ev = eval(id, curr, newState, false, curr.next.isEmpty)
        newState = ev._2
        if (ev._1 != prev.next.get.contains(stateVariablesFuture.indexOf(id) + 1)) {
          //println(s"CompChk: ${pp(prev)} -> ${pp(curr)} resulted false at $id")
          return (false, newState)
        }
    }

    val res = assumptionCheck(curr, newState, false, curr.next.isEmpty)

    //println(s"CompChk: ${pp(prev)} -> ${pp(curr)} resulted ${res._1}")
    res
  }

  /**
   * Evaluate an expression for the given state
   * @param exp The expression to be evaluated
   * @param curr The current state on which the calculation is based
   * @param state The cached tate containing previous calculation results
   * @param beginning If the evaluation is at the beginning of the trace
   * @param end If the evaluation is at the end of the trace
   * @return The result and a Map of cached intermediate results (i.e. the new cache based on state)
   */
  def eval(exp: Expression, curr: State, state: Map[Identifier, Boolean], beginning: Boolean = false, end: Boolean = false): (Boolean, Map[Identifier, Boolean]) = {
    exp match {
      case AST.Application(op, exps) => {
        var newState = state
        val vals = exps.map(e => {
          val res = eval(e, curr, newState, beginning, end)
          newState = res._2
          res._1
        })
        (CertainEvaluator.applyOp(op, vals.map(BoolValue(_))).asInstanceOf[BoolValue].v, newState) //TODO: Overhead!
      }
      case AST.Offset(i: Identifier, -1, BoolValue(value)) => (if (beginning) value else curr.curr.contains(stateVariables.indexOf(i) + 1), state)
      case AST.Offset(i: Identifier, 1, BoolValue(value)) => (if (end) value else curr.next.get.contains(stateVariablesFuture.indexOf(i) + 1), state)
      case AST.Offset(exp, 0, _) => eval(exp, curr, state, beginning, end)
      case AST.Constant(BoolValue(value)) => (value, state)
      case id: AST.Identifier if spec.inputs.contains(id.name) => (curr.curr.contains(stateVariables.indexOf(id) + 1), state)
      case id: AST.Identifier =>
        if (state.contains(id)) {
          (state(id), state)
        } else {
          val (v, s) = eval(spec.definitions(id), curr, state, beginning, end)
          (v, s + (id -> v))
        }
      case _ => throw new Errors.InternalException("Severe error during future investigation", panic = true)
    }
  }

  /**
   * Check for a given state if the assumption (if existent) is satisfied.
   * The assumption is the result of the stream ASSUMPTION.
   * @param curr The state to be checked
   * @param state The cached state for evaluations
   * @param beginning If the state is the first in the trace
   * @param end If the state is the last in the trace
   * @return Result of the assumption check and new cache
   */
  def assumptionCheck(curr: State, state: Map[Identifier, Boolean], beginning: Boolean = false, end: Boolean = false) : (Boolean, Map[Identifier, Boolean]) = {
    if (spec.definitions.contains(assumption)) {
      val r = eval(assumption, curr, state, beginning, end)
      r
    } else {
      (true, state)
    }
  }

}
