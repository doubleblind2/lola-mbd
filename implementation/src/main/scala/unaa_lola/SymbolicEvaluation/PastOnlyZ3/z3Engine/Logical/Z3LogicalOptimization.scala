package unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.Logical

import com.microsoft.z3.BoolExpr
import unaa_lola.AST.{Identifier, SupIdentifier}
import unaa_lola.Value.Bool
import unaa_lola.SymbolicEvaluation.z3Engine.LinAlgCore.newVarId
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.LinAlg.Z3LinAlgFormula
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.{Formula, FormulaOptimization}
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.{Logical, Z3QueryEnvironment}
import unaa_lola.util.{Errors, Logging}

/**
 * Object containing functions for simplification of logical formulas via basis change
 */
object Z3LogicalOptimization extends FormulaOptimization[Formula[Option[Boolean]]] {

  /**
   * Simplifies a system of logical equations by performing a basis change
   *
   * @param lfs Sequence of the equations as [[Z3LogicalFormula]]s
   * @param spare Identifiers to be preserved (are not replaced by others)
   * @param force Force calculation of basis of linearly independent vectors
   * @return New equations as [[Z3LogicalFormula]]s in the same order as the lfs parameter
   */
  override def simplify(lfs: Seq[Formula[Option[Boolean]]], spare: Set[SupIdentifier] = Set(), force: Boolean = false): Seq[Z3LogicalFormula] = {
    try {
      Logging.log(s"Start Boolean simplification: System: $lfs Spare vars: $spare", channel = Logging.DEBUG)
      val res = intSimplify(lfs.asInstanceOf[Seq[Z3LogicalFormula]], spare, force)
      Logging.log(s"Simplification result: $res", channel = Logging.DEBUG)
      res
    } catch {
      case t : Throwable =>
        println(t)
        t.printStackTrace()
        throw new Errors.InternalException(s"Exception during simplification: $t")
    }
  }

  private def intSimplify(lfs: Seq[Z3LogicalFormula], spare: Set[SupIdentifier] = Set(), force: Boolean = false): Seq[Z3LogicalFormula] = {
    if (lfs.isEmpty) return Seq()

    val env = Z3LogicalFormula.getEnv(lfs)
    val fact = env.getLogicalFormulaFactory
    val spareSeq = spare.toSeq

    if (spare.isEmpty) {
      intSimplifyNoSpare(lfs, env)
    } else {
      (0 until Math.pow(2, spare.size).toInt).map {
        i =>
          val setting =  (0 until spare.size).map {
            j => (spareSeq(j) -> ((i & Math.pow(2, j).toInt) != 0))
          }.toMap
          val sr = intSimplifyNoSpare(lfs, env, setting)
          sr.map(lf => fact.and(lf, new Logical.Z3LogicalFormula(spare, Z3QueryEnvironment.buildZ3Expression(spareSeq, BigInt(1 << i), env.ctx), env)))
      }.reduce[Seq[Z3LogicalFormula]]{case (fs1, fs2) => fs1.zip(fs2).map{case (f1, f2) => fact.or(f1, f2)}}
    }
  }

  private def intSimplifyNoSpare(lfs: Seq[Z3LogicalFormula], env: Z3QueryEnvironment, setting: Map[SupIdentifier, Boolean] = Map()): Seq[Z3LogicalFormula] = {
    val ctx = env.ctx
    var resVect : Set[Int] = Set()
    val assump = if (setting.isEmpty)
      ctx.mkTrue()
    else
      setting.map{case (id, b) =>
        val f = Z3QueryEnvironment.buildZ3Expression(id, ctx).asInstanceOf[BoolExpr]
        if (b) f else ctx.mkNot(f)
      }.reduce[BoolExpr]{case (e1, e2) => ctx.mkAnd(e1, e2)}

    (0 until Math.pow(2, lfs.size).toInt).foreach {
      i =>
        if (env.checkConformance(lfs.map(_.z3Shortcut), i, assump)) {
          resVect += i
        }
    }

    if (resVect.isEmpty) return lfs.map(_ => env.getLogicalFormulaFactory.falseVal)

    val resVectSeq = resVect.toSeq

    val newBase = (0 until Math.ceil(Math.log(resVect.size) / Math.log(2)).toInt).map{_ => s"PRUN#$newVarId"}

    var results = lfs.indices.map(_ => 0)

    var currVect = 0
    (0 until Math.pow(2, newBase.size).toInt).foreach{
      i =>
        currVect = if (i < resVect.size) resVectSeq(i) else resVectSeq.last
        results = results.map{
          bi =>
            val r = (bi << 1) + currVect % 2
            currVect = currVect >> 1
            r
        }
    }

    val r = results.map{
      i =>
        val basis = newBase.map(Identifier(_, Bool))
        val exp = Z3QueryEnvironment.buildZ3Expression(basis, i, ctx)
        new Z3LogicalFormula(basis.toSet ++ setting.keys, exp, env)
    }

    r
  }

}
