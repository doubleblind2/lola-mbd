package unaa_lola.BooleanFuture

import unaa_lola.BooleanFuture.BooleanFutureEvaluator.State
import unaa_lola.util.Logging

/**
 * Class to exhaustively search the state space for the future evaluation of a given specification.
 * The emptiness check is performed directly during initialization of this class.
 * @param bc The [[BooleanFutureCalculationCore]] instance used for the computation. It holds the reference to the
 *           specification under examination.
 */
class EmptinessCheck(bc: BooleanFutureCalculationCore) {

  private var rootMap: Map[State, Int] = Map()
  private var components: Map[Int, (Boolean, Set[State])] = Map()
  private var validInitialStates: Set[State] = Set()

  private val inputCombs = bc.inputCombs
  private val futureGuessings = bc.futureGuessings

  private var dfsCnt = 0
  private def dfsCounter(): Int = {dfsCnt += 1; dfsCnt}

  private def pp(s: State): String = bc.pp(s)

  private def startDFS() : Unit = {
    inputCombs.foreach{
      ic =>
        futureGuessings.foreach{
        fg =>
          val state = State(ic, Some(fg))
          Logging.log(s"Exploration for initial state: ${pp(state)}", channel = Logging.DEBUG)
          val ares = bc.assumptionCheck(state, Map())

          if (ares._1) {

            var i = 1
            futureGuessings.foreach{ guess =>
              i = i + 1
              val fuStates = bc.followUpStates(state, guess, ares._2, true, true)
              val sres = fuStates.map(s => performDFS(Set(), s, state)).foldLeft[Boolean](false) { (c, dfsRes) => c || dfsRes._1 }

              if (sres) validInitialStates += state
            }

          } else {
            Logging.log(s"Assumption broken in: ${pp(state)}", channel = Logging.DEBUG)
          }
      }
    }
  }

  private def performDFS(stack: Set[Int], curr: State, prev: State): (Boolean, Int) = {
    val myId = dfsCounter()
    Logging.log(s"dfs(${pp(curr)},${pp(prev)}   Stack: $stack @$myId", channel = Logging.DEBUG)

    val (v, cache) = bc.checkStatesCompatible(curr, prev, Map())
    if (!v) {
      Logging.log("XX No sound state transition", channel = Logging.DEBUG)
      return (false, Integer.MAX_VALUE)
    }
    if (rootMap.contains(curr) && stack.contains(rootMap(curr))) {
      Logging.log("X Stack state or adjacent to stack state!", channel = Logging.DEBUG)
      return (false, rootMap(curr))
    }
    if (!rootMap.contains(curr)) {
      rootMap += (curr -> myId)
      components += (myId -> (false, Set(curr)))

        if (curr.next.isEmpty) {
          Logging.log("VVV Final state", channel = Logging.DEBUG)
          components += (myId -> (true, Set(curr)))
          return (true, myId)
        }

        Logging.log("VV Sound state transition", channel = Logging.DEBUG)
        val fuStates = futureGuessings.flatMap(guess => bc.followUpStates(curr, guess, cache, false, true))

        val (fs, id) = fuStates.foldLeft[(Boolean, Int)]((false, myId)){case ((b, i),next) =>
          val dfs = performDFS(stack + myId, next, curr)
          (b || dfs._1, Integer.min(i, dfs._2))
        }
        Logging.log(s"allres ${pp(curr)} --> $fs", channel = Logging.DEBUG)
        rootMap += (curr -> id)
        if (myId != id) {
          components(myId)._2.foreach(s => rootMap += (s -> id))
          components += (id -> (false, components(id)._2 ++ components(myId)._2))
          components = components.removed(myId)
        } else {
          components += (id -> (fs, components(id)._2))
        }
        (fs, id)
    } else {
        Logging.log(s"X Visited state!: ${stateNonEmpty(curr, false)}", channel = Logging.DEBUG)
        (stateNonEmpty(curr, false), Integer.MAX_VALUE)
    }
  }

  val from = System.nanoTime();
  startDFS()
  val to = System.nanoTime();

  Logging.log(s"BooleanFuture.EmptynessCheck done! Took: ${(to - from) / 1000} μs")
  Logging.log(nonEmptyStates().map(_.prettyPrint(bc.stateVariables, bc.stateVariablesFuture)).toString(), channel = Logging.DEBUG)
  Logging.log(validInitialStates.map(_.prettyPrint(bc.stateVariables, bc.stateVariablesFuture)).toString(), channel = Logging.DEBUG)

  /**
   * Check if a state is not empty (i.e. may lead to a valid final state)
   * This function only delivers correct results for states which are actually reachable in an evaluation.
   * No real check is performed, rather a cached result is looked up.
   * @param s The state to be checked
   * @param initial If s shall be considered as intial state (one with all previous values set to false)
   * @return The result of the emptyness check
   */
  def stateNonEmpty(s: State, initial: Boolean): Boolean = {
    if (initial) validInitialStates(s) else rootMap.contains(s) && components(rootMap(s))._1
  }

  /**
   * Returns a list of non empty states (i.e. may lead to a valid final state) which are reachable from valid initial
   * states. The returned list does not include initial states.
   * @return List of non-empty states
   */
  def nonEmptyStates() : Set[State] = components.flatMap{
    case (_,(true, stateSet)) => stateSet
    case _ => Set()
  }.toSet

}
