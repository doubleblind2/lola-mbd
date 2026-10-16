
//TODO documentation
package unaa_lola.SymbolicEvaluation.z3Engine.Diagnosis

import unaa_lola.AST.Identifier
import unaa_lola.SymbolicEvaluation.z3Engine.Z3CombiQuery
import com.microsoft.z3.BoolExpr
import unaa_lola.util.Errors.InternalException
import unaa_lola.AST.SymbolIdentifier
import unaa_lola.SymbolicEvaluation.z3Engine.Z3CombiConstraintState
import unaa_lola.util.Logging
import unaa_lola.AST.SupIdentifier
import collection.mutable.PriorityQueue
import unaa_lola.SymbolicEvaluation.z3Engine.Diagnosis.Z3Diagnosis._
import unaa_lola.Config

object Z3Diagnosis {

  def tsFree(diags: Set[Set[SymbolIdentifier]]): Set[Set[Identifier]] = {
    diags.map(_.map{case SymbolIdentifier(baseId, ts) => baseId})
  }

  def isSubsetOf(multiDiags: Set[Set[Identifier]], tempDiags: Set[Set[SymbolIdentifier]]): Boolean = {
    multiDiags.subsetOf(tsFree(tempDiags))
  }

  def diagsToString[C](diags: Set[Set[C]]): String = s"{${diags.map(diagToString).mkString(",")}}"

  def diagToString[C](diag: Set[C]): String = s"{${diag.map(_.toString).mkString(",")}}"

  def containsEmpty[C](diag: Set[Set[C]]): Boolean = diag.contains(Set())

  def containsSingleCompDiag(singleCompDiag: Set[Identifier], tempDiags: Set[Set[SymbolIdentifier]]): Boolean = {
    tsFree(tempDiags).contains(singleCompDiag)
  }

}

class Z3Diagnosis(components: Set[Identifier], c: Config) {

  Logging.log(s"Use $components as components for diagnosis", channel = Logging.DEBUG)

  def doDiagnosis(cs: Z3CombiConstraintState, keep: Int): Unit = {
    val diagFrom = System.nanoTime()

    if (c.singleDiag) {
      val single = zeroInstantDiagnosis(cs)
      Logging.log(s"0-instant diagnoses for ts=${cs.ts}: ${diagsToString(single)}")
      println(diagsToString(single))
      Logging.log(s"${cs.ts},Single,${if (c.specificAtempDiag.isDefined && !containsEmpty(single)) s"${diagToString(c.specificAtempDiag.get)}=" + single.contains(c.specificAtempDiag.get) else ""},${diagsToString(single)}", channel = Logging.DIAGNOSES)
    }
    val diagTo1 = System.nanoTime()
    //Logging.log(s"0-instant diagnosis took ${(diagTo1 - diagFrom) / 1000} µs")

    if (c.multiDiag) {
      val multi = kInstantDiagnosis(cs, keep)
      Logging.log(s"$keep-instant diagnoses for ts=${cs.ts}: ${diagsToString(multi)}")
      println(diagsToString(multi))
      Logging.log(s"${cs.ts},Multi,${if (c.specificAtempDiag.isDefined && !containsEmpty(multi)) s"${diagToString(c.specificAtempDiag.get)}=" + multi.contains(c.specificAtempDiag.get) else ""},${diagsToString(multi)}", channel = Logging.DIAGNOSES)
    }
    val diagTo2 = System.nanoTime()
    //Logging.log(s"$keep-instant diagnosis took ${(diagTo2 - diagTo1) / 1000} µs")

    if (c.temporalDiag) {
      val temp = kTemporalDiagnosis(cs, keep)
      Logging.log(s"$keep-temporal diagnoses for ts=${cs.ts}: ${diagsToString(temp)}")
      println(diagsToString(temp))
      Logging.log(s"${cs.ts},Temp,${if (c.specificTempDiag.isDefined && !containsEmpty(temp)) s"${diagToString(c.specificTempDiag.get)}=" + temp.contains(c.specificTempDiag.get) else ""},${diagsToString(temp)}", channel = Logging.DIAGNOSES)
      //Logging.log(s"${cs.ts},multiIsSubsetOfTemp=${isSubsetOf(multi, temp)}", channel = Logging.DIAGNOSES)
    }
    val diagTo3 = System.nanoTime()
    //Logging.log(s"$keep-temporal diagnosis took ${(diagTo3 - diagTo2) / 1000} µs")

    Logging.log(s"${diagTo1 - diagFrom},${diagTo2 - diagTo1},${diagTo3 - diagTo2}", channel = Logging.DIAG_RUNTIMES)
  }

  def zeroInstantDiagnosis(cs: Z3CombiConstraintState): Set[Set[Identifier]] = {
    hsTree(components, checkAtemporalConflict(_, cs, 0))
  }

  def kInstantDiagnosis(cs: Z3CombiConstraintState, k: Int): Set[Set[Identifier]] = {
    val cutoffTs = if (k > cs.ts) cs.ts else k
    hsTree(components, checkAtemporalConflict(_, cs, cutoffTs))
  }

  def kTemporalDiagnosis(cs: Z3CombiConstraintState, k: Int): Set[Set[SymbolIdentifier]] = {
    val cutoffTs = if (k > cs.ts) cs.ts else k
    val timedComponents = Range(-cutoffTs, 1).flatMap(ts => components.map(SymbolIdentifier(_, ts))).toSet
    hsTree(timedComponents, checkTemporalConflict(_, cs))
  }

  private def checkAtemporalConflict(c: Set[Identifier], cs: Z3CombiConstraintState, untilPast: Int): Boolean = {
    Logging.log(s"Check if conflict for components $c for past $untilPast instants", channel = Logging.DEBUG)    
    val mustNotHold = Range(-untilPast, 1).flatMap(ts => c.map(SymbolIdentifier(_, ts))).toSet
    !queryCall(Set(), mustNotHold, cs)
  }

  private def checkAtemporalDiagnosis(diag: Set[Identifier], cs: Z3CombiConstraintState, untilPast: Int): Boolean = {
    Logging.log(s"Check if diagnosis for components $diag for past $untilPast instants", channel = Logging.DEBUG)
    def toSymbols(ids: Set[Identifier]): Set[SymbolIdentifier] = Range(-untilPast, 1).flatMap(ts => ids.map(SymbolIdentifier(_, ts))).toSet
    
    val mustHold = toSymbols(diag)
    val mustNotHold = toSymbols(components.removedAll(diag))

    queryCall(mustHold, mustNotHold, cs)
  }

  private def checkTemporalConflict(c: Set[SymbolIdentifier], cs: Z3CombiConstraintState): Boolean = {
    Logging.log(s"Check if conflict for components $c", channel = Logging.DEBUG)
    !queryCall(Set(), c, cs)
  }

  private def checkTemporalDiagnosis(diag: Set[SymbolIdentifier], cs: Z3CombiConstraintState, allTimedComps: Set[SymbolIdentifier]): Boolean = {
    Logging.log(s"Check if diagnosis for components $diag", channel = Logging.DEBUG)
    queryCall(diag, allTimedComps.removedAll(diag), cs)
  }

  private def queryCall(mustHold: Set[SymbolIdentifier], mustNotHold: Set[SymbolIdentifier], cs: Z3CombiConstraintState): Boolean = {
    val ctx = cs.ctx
    def toBoolExpr(symbols: Set[SymbolIdentifier]): Set[BoolExpr] = symbols.map(_.toString).map(ctx.mkBoolConst)

    val trueConstraints = if (mustHold.isEmpty) ctx.mkTrue() else toBoolExpr(mustHold).reduce((a, b) => ctx.mkAnd(a, b))
    val falseConstraints = if (mustNotHold.isEmpty) ctx.mkTrue() else toBoolExpr(mustNotHold).map(ctx.mkNot).reduce((a, b) => ctx.mkAnd(a, b))
    val addConstraints = ctx.mkAnd(trueConstraints, falseConstraints)
    
    Logging.log(s"Call query with additional constraint $addConstraints", channel = Logging.DEBUG)
    cs.getQuery.boolQuery(addConstraints) match {
      case None | Some(true) => true
      case Some(false) => false
    }
  }

  private def simpleDfs[C](comps: Set[C], check: Set[C] => Boolean): Set[Set[C]] = {
    var cache: Map[Set[C], Boolean] = Map()
    def cached(c: Set[C]): Boolean = {
      if (!cache.contains(c)) {
        val res = check(c)
        cache += (c -> res)
        res
      } else {
        cache(c)
      }
    }

    var minimals: Set[Set[C]] = Set()

    def dfs(current: Set[C], remaining: Seq[C]): Unit = {
      if (!minimals.exists(_.subsetOf(current))) {
        if (cached(current)) {
          minimals = minimals.filter(!current.subsetOf(_)) + current
        } else if (remaining.nonEmpty) {
          val (h,t) = (remaining.head, remaining.tail)
          dfs(current + h, t)
          dfs(current, t)
        }
      }
    }

    dfs(Set.empty, comps.toSeq)
    minimals
  }

  private def quickXplain[C](a: Set[C], b: Set[C], p: Set[C] => Boolean): Option[Set[C]] = {
    if (!p(a ++ b)){
      None
    } else if (a.isEmpty) {
      Some(Set())
    } else { 
      Some(quickXplainRec(b, a, b, p))
    }
  } 

  private def quickXplainRec[C](c: Set[C], a: Set[C], b: Set[C], p: Set[C] => Boolean): Set[C] = {
    if (c.nonEmpty && p(b)) {
      Set()
    } else if (a.size == 1) {
      a
    } else {
      val (a1, a2) = a.splitAt(a.size / 2)
      val x2 = quickXplainRec(a1, a2, b ++ a1, p)
      val x1 = quickXplainRec(x2, a1, b ++ x2, p)
      x1 ++ x2
    }
  }

  private def hsTree[C](comps: Set[C], p: Set[C] => Boolean): Set[Set[C]] = {
    var dCalc = Set[Set[C]]()
    var cCalc = Set[Set[C]]()
    val ordering = Ordering.by[Set[C], Int](_.size).reverse
    val queue = PriorityQueue[Set[C]](Set())(ordering)
  
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      if (!dCalc.exists(_.subsetOf(node)) && !queue.exists(_ == node)) {
        cCalc.find(_.intersect(node).isEmpty) match {
          case Some(c) => queue ++= c.map(node + _)
          case None =>
            quickXplain(comps -- node, Set(), p) match {
              case None => dCalc += node
              case Some(c) => {
                cCalc += c
                queue ++= c.map(node + _)
              }
            }
        }
      }
    }
    dCalc
  }

  private def hsTreeOriginal[C](comps: Set[C], p: Set[C] => Boolean): Set[Set[C]] = {
    var dCalc = Set[Set[C]]()
    var cCalc = Set[Set[C]]()
    val ordering = Ordering.by[Set[C], Int](_.size).reverse
    val queue = PriorityQueue[Set[C]](Set())(ordering)
  
    while (queue.nonEmpty) {
      val node = queue.dequeue()
      val (conflictLabel, newcCalc) = label(comps, node, cCalc, dCalc, queue, p)
      cCalc = newcCalc
      conflictLabel match {
       case Valid => dCalc += node
       case Closed => 
       case Open(c: Set[C]) => c.foreach(e => queue.enqueue(node + e))
      }
    }
    dCalc
  }

  private def label[C](comps: Set[C], node: Set[C], cCalc: Set[Set[C]], dCalc: Set[Set[C]], queue: PriorityQueue[Set[C]], p: Set[C] => Boolean): (ConflictLabel, Set[Set[C]]) = {
    if (dCalc.exists(_.subsetOf(node)) || queue.exists(_ == node)) {
      (Closed, cCalc)
    } else {
      cCalc.find(_.intersect(node).isEmpty) match {
        case Some(c) => (Open(c), cCalc)
        case None =>
          quickXplain(comps -- node, Set(), p) match {
            case None => (Valid, cCalc)
            case Some(c) => (Open(c), cCalc + c)
          }
      }
    }
  }

  sealed trait ConflictLabel
  case object Valid extends ConflictLabel
  case object Closed extends ConflictLabel
  case class Open[C](c: Set[C]) extends ConflictLabel

}