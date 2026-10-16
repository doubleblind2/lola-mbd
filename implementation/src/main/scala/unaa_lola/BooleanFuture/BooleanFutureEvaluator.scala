package unaa_lola.BooleanFuture

import unaa_lola.AST.{Identifier, ExactSpecification}
import unaa_lola.BooleanFuture.BooleanFutureEvaluator.State
import unaa_lola.util.{Errors, Logging}
import unaa_lola.{AST, Evaluator}
import unaa_lola.Value._

import scala.collection.BitSet
import unaa_lola.Config

/**
 * Object containing some static definitions for Future Boolean LOLA evaluation
 */
object BooleanFutureEvaluator {

  type FutureVals = BitSet
  type CurrVals = BitSet

  /**
   * Class representing states of the LOLA monitor.
   * It maintains
   * * the current value of input streams (in curr)
   * * the previous value of streams accessed by -1 offsets in the spec (in curr)
   * * the guess for the next value of streams accessed by +1 offsets in the spec (in next)
   * @param curr Bitset of all input streams which are currently true, and all streams which have value true in the last
   *             step and are referenced by -1 in the spec
   * @param next Bitset of all streams which have value true in the next step and are referenced by +1 in the spec
   *             or None if the state is final (i.e. the trace end is reached)
   */
  case class State(curr: CurrVals, next: Option[FutureVals]) {

    /**
     * Produces a pretty representation of a state as string
     * @param cNames List of streams potentially stored in curr in the correct order
     * @param nNames List of streams potentially stored in next in the correct order
     * @return Pretty string representation of the state
     */
    def prettyPrint(cNames: Seq[Identifier], nNames: Seq[Identifier]): String = {
      def nameVect(s: Seq[Identifier]): Seq[String] = s.map(_.name)
      def valuesVector(n: Int, v: BitSet): Seq[String] = if (n == 0) Seq[String]() else valuesVector(n-1, v) :+ (if (v.contains(n)) "1" else "0")
      def pp(s: Seq[Identifier], v: BitSet): String = nameVect(s).zip(valuesVector(s.length, v)).map{case (s1, s2) => s"$s1=$s2"}.mkString(", ")

      s"{Curr: ${pp(cNames, curr)}" + (if (next.isEmpty) " --final-- }" else s" | Future: ${pp(nNames, next.get)}}")
    }

  }
}

/**
 * [[EvaluatorInterface]] implementation for Boolean LOLA with future references
 * @param spec The specification to be evaluated
 * @param config A configuration
 */
class BooleanFutureEvaluator(spec: ExactSpecification, config: Config) extends Evaluator(spec, config, false, ultraFlat = true) {

  private val bc = new BooleanFutureCalculationCore(flatspec)
  private val ec = new EmptinessCheck(bc)
  private var powerState : Set[State] = Set()
  private var nextPowerState : Set[State] = Set()
  private var calculatedOutputs : Map[Identifier, Set[Boolean]] = Map()
  private var consumedInputs : Set[BitSet] = Set()

  private def getOutputRep(v: Set[Boolean]): Value = {
    if (v.isEmpty) {
      throw new Errors.InternalException("Broken state!")
    } else if (v == Set(true, false)) {
      UnknownBoolValue()
    } else {
      BoolValue(v.head)
    }
  }

  /** @inheritdoc */
  override def eval(): Unit = {
    consumedInputs = Set(BitSet())
    nextPowerState = Set()

    consumeInputs.foreach{case (stream, value) => readInputVal(stream, value)}
    calculatedOutputs = Map()

    val from = System.nanoTime();

    //println(bc.stateVariables)

    if (ts == 0) {
      bc.futureGuessings.foreach {
        fg =>
          consumedInputs.foreach {
            inputs =>
              val s = State(inputs, Some(fg))
              //println(s"check ${bc.pp(s)}")
              if (ec.stateNonEmpty(s, true)) {
                //println("Add!")
                powerState += s
              }
          }
      }
    } else {
      powerState.foreach {
        curr =>
          var calcState : Map[Identifier, Boolean] = Map()

          val newStateVars = bc.stateVariablesPast.foldLeft[BitSet](BitSet()) {
            case (c, id) =>
              val res = bc.eval(id, curr, calcState, ts == 1)
              calcState = res._2
              if (res._1) c + (bc.stateVariables.indexOf(id) + 1) else c
          }

          bc.futureGuessings.foreach {
            fg =>
              consumedInputs.foreach {
                inputs =>

                val fus = State(inputs | newStateVars, Some(fg))

                if (!nextPowerState.contains(fus) && ec.stateNonEmpty(fus, false)) {
                  val r = bc.checkStatesCompatible(fus, curr, Map())
                  if (r._1) {
                    nextPowerState += fus
                    Logging.log(s"${bc.pp(curr)} --> ${bc.pp(fus)} || ADDED", channel = Logging.DEBUG)
                  }
                  Logging.log(s"${bc.pp(curr)} --> ${bc.pp(fus)} || state not compatible", channel = Logging.DEBUG)
                }
                else Logging.log(s"${bc.pp(curr)} --> ${bc.pp(fus)} || second either already contained or empty", channel = Logging.DEBUG)
              }
          }
      }

      powerState = nextPowerState
    }

    Logging.log(s"Transferred to states: ${powerState.map(bc.pp).mkString(",")}")

    if (powerState.isEmpty) {
      throw new Errors.InternalException("Set of active states has run empty! May be due to a broken assumption?", panic = true)
    }

    powerState.foreach {
      curr =>
        var calcState: Map[Identifier, Boolean] = Map()

        flatspec.outputs.foreach {
          id =>
            if (!calculatedOutputs.contains(id) || calculatedOutputs(id).size != 2) {
              val res = bc.eval(id, curr, calcState, ts == 0)
              calcState = res._2
              calculatedOutputs += (id -> (calculatedOutputs.getOrElse(id, Set()) + res._1))
            }
        }
    }

    flatspec.outputs.foreach {
      o => singleValOutput(o, getOutputRep(calculatedOutputs(o)))
    }
    val to = System.nanoTime();

    Logging.log(s"Evaluation took ${(to - from) / 1000} μs")
    Logging.log(s"${(to - from) / 1000}", channel = Logging.RUNTIMES)
  }

  def readInputVal(stream: Identifier, value: Value): Unit = {
    value match {
      case BoolValue(v) => if (v) consumedInputs = consumedInputs.map(s => s + (bc.stateVariables.indexOf(stream) + 1))
      case UnknownBoolValue(_) => consumedInputs ++= consumedInputs.map(s => s + (bc.stateVariables.indexOf(stream) + 1))
      case _ => throw new InternalError(s"Value $value not supported by BooleanFutureEvaluator")
    }
  }
}
