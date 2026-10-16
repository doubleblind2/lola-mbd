package unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.LinAlg

import breeze.linalg.{DenseMatrix, DenseVector}
import unaa_lola.AST.{Identifier, SupIdentifier}
import unaa_lola.Value.Real
import unaa_lola.SymbolicEvaluation.z3Engine.LinAlgCore.newVarId
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.{Formula, FormulaOptimization}
import unaa_lola.SymbolicEvaluation.PastOnlyZ3.z3Engine.Z3QueryEnvironment
import unaa_lola.util.Common.OInterval
import unaa_lola.util.{Errors, Logging}

import scala.annotation.tailrec

/**
 * Object containing functions for simplification of linear algebra formulas via basis change resp. vector identification
 */
object Z3LinAlgOptimization extends FormulaOptimization[Formula[OInterval]] {

  /**
   * Trigger pre-loading of breeze framework to avoid long loading time during evaluation
   *
   * @return A dummy vector
   */
  def loadFramework: DenseVector[Double] = {
    val v = DenseVector.zeros[Double](3)
    val m = DenseMatrix.ones[Double](3, 3)
    m * v
  }

  /**
   * Creates a breeze vector of size n with a single 1 at position i and 0s at all other positions
   *
   * @param i The position in the vector with a 1
   * @param n Size of vector
   * @return Vector with a single 1 at position i and 0s at all other positions
   */
  private def oneVect(i: Int, n: Int): DenseVector[Double] = {
    val o = DenseVector.zeros[Double](n)
    o(i) = 1
    o
  }

  /**
   * Translates a dense vector and a belonging Identifier basis and offset to the corresponding [[Z3LinAlgFormula]]
   *
   * @param v        Vector with coefficients
   * @param basisIDs Identifiers corresponding to the vector entries
   * @param offset   Offset value
   * @param env   Corresponding z3 environment
   * @return The LinAlgFormula
   */
  private def vectorToLinAlgFormula(v: DenseVector[Double], basisIDs: Seq[SupIdentifier], offset: Double, env: Z3QueryEnvironment): Z3LinAlgFormula = {
    new Z3LinAlgFormula(basisIDs.zipWithIndex.flatMap { case (id, idx) => if (v(idx) != 0) Some(id.asInstanceOf[Identifier] -> v(idx)) else None }.toMap, offset, env)
  }

  /**
   * Simplifies a system of linear algebra equations by performing a basis change
   *
   * If the number of variables in the equations is lower than three times the number
   * of equations only identical vectors of the multiplication matrix are unified
   * (this guarantees no loss in precision even if the variables have a bounded value range)
   *
   * Otherwise a new basis of linearly independent vectors is built.
   *
   * @param lfs   Sequence of the equations as [[Z3LinAlgFormula]]s
   * @param spare   Identifiers to be preserved (are not replaced by others)
   * @param force Force calculation of basis of linearly independent vectors
   * @return New equations as [[Z3LinAlgFormula]]s in the same order as the lfs parameter
   */
  override def simplify(lfs: Seq[Formula[OInterval]], spare: Set[SupIdentifier] = Set(), force: Boolean = false): Seq[Z3LinAlgFormula] = {
    try {
      intSimplify(lfs.asInstanceOf[Seq[Z3LinAlgFormula]], spare, force)
    } catch {
      case t : Throwable => throw new Errors.InternalException(s"Exception during simplification: $t")
    }
  }


  private def intSimplify(lfs: Seq[Z3LinAlgFormula], spare: Set[SupIdentifier], force: Boolean = false): Seq[Z3LinAlgFormula] = {

    if (lfs.isEmpty) return Seq()

    val env = Z3LinAlgFormula.getEnv(lfs)

    val parts = lfs.map(f => (Z3LinAlgFormula(f.coeff.filter{case (id, _) => !spare.contains(id)}, 0, env),
                              Z3LinAlgFormula(f.coeff.filter{case (id, _) => spare.contains(id)}, f.offset, env)))

    val simpPart = parts.map(_._1)
    val nonSimpPart = parts.map(_._2)

    val basis = simpPart.foldLeft[Seq[Identifier]](Seq()) {
      case (c, laf) => c ++ laf.coeff.keys
    }.distinct.sortWith {
      case (b1, b2) =>
        val i1 = env.boundQuery(b1)
        val i2 = env.boundQuery(b2)

        val r1 = if (i1._1.isDefined && i1._2.isDefined) i1._2.get - i1._1.get else Double.MaxValue
        val r2 = if (i2._1.isDefined && i2._2.isDefined) i2._2.get - i2._1.get else Double.MinValue

        r1 > r2
    }

    val colVectors: Seq[DenseVector[Double]] = basis.map {
      i =>
        simpPart.foldLeft[Seq[Double]](Seq()) {
          case (c, laf) => c :+ laf.coeff.getOrElse(i, 0)
        }
    }.map(DenseVector(_: _*))


    val (newBasis, transposeMatrix) =
      if (basis.size <= 3 * simpPart.size && !force) {
        duplicateElimination(colVectors, simpPart.size)
      } else {
        gaussJordan(colVectors, simpPart.size)
      }

    val newBasisIDs = newBasis.map(_ => Identifier(s"r#$newVarId", Real))

    newBasisIDs.zipWithIndex.foreach {
      case (id, idx) =>
        adjustMinMax(id, transposeMatrix(idx, ::).t, basis, env)
    }

    simpPart.zipWithIndex.map {
      case (laf, idx) =>
        vectorToLinAlgFormula(DenseVector(newBasis.map(v => v(idx)): _*), newBasisIDs, laf.offset, env)
    }.zip(nonSimpPart).map{case (f1, f2) => env.getLinAlgFormulaFactory.plus(f1, f2)}

  }


  /**
   * Eliminates all vectors from vects which are a multiple of an already contained vector
   *
   * @param vects Sequence of vectors to be simplified
   * @param n Dimension of vectors
   * @return Tuple of sequence of new vectors and a matrix describing the basis change
   */
  private def duplicateElimination(vects: Seq[DenseVector[Double]], n: Int): (Seq[DenseVector[Double]], DenseMatrix[Double]) = {

    def firstIdx(v: DenseVector[Double]): Double = {
      val i = (0 to n).find(i => v(i) != 0)
      v(i.get)
    }

    val m = vects.size

    var basis: Seq[DenseVector[Double]] = Seq()
    val transposeMatrix: DenseMatrix[Double] = DenseMatrix.zeros(m, m)

    vects.zipWithIndex.foreach {
      case (v, i) =>
        val vc = firstIdx(v)
        val alt = basis.zipWithIndex.find {
          b =>
            val bc = firstIdx(b._1)
            (v - vc / bc * b._1) == DenseVector.zeros[Double](n)
        }

        if (alt.isDefined) {
          transposeMatrix(::, i) := oneVect(alt.get._2, m) * (vc / firstIdx(alt.get._1))
        } else {
          basis :+= v
          transposeMatrix(::, i) := oneVect(basis.size - 1, m)
        }
    }

    (basis, transposeMatrix(basis.indices, ::))

  }


  /**
   * Applies Gauss/Jordan algorithm to find a minimal basis for a given set of vectors
   *
   * @param vects Sequence of vectors to be simplified
   * @param n Dimension of vectors
   * @return Tuple of sequence of new vectors and a matrix describing the basis change
   */
  private def gaussJordan(vects: Seq[DenseVector[Double]], n: Int): (Seq[DenseVector[Double]], DenseMatrix[Double]) = {
    val m = vects.size

    var internalBasis: Map[Int, DenseVector[Double]] = Map()
    var basis: Seq[DenseVector[Double]] = Seq()
    var basisCreation: Map[Int, DenseVector[Double]] = Map()
    val transposeMatrix: DenseMatrix[Double] = DenseMatrix.zeros(m, m)


    def resMin: Int = {
      (0 to n).find(!internalBasis.contains(_)).getOrElse(n)
    }

    def numLeadingZeros(v: DenseVector[Double]): Int = {
      var lz = 0
      v.data.foreach {
        e =>
          if (e == 0) lz += 1 else return lz
      }
      lz
    }

    @tailrec
    def stripVector(i: Int, v: DenseVector[Double], coeff: DenseVector[Double]): (Boolean, DenseVector[Double]) = {
      val lz = numLeadingZeros(v)

      if (lz == n) {
        //Lin. comb possible
        (false, coeff)
      } else {
        if (lz >= resMin && !internalBasis.contains(lz)) {
          internalBasis += (lz -> v)
          val newCoeff = oneVect(i, m) - coeff
          basisCreation += (lz -> newCoeff)
          //Lin. comb not possible
          (true, newCoeff)
        } else {
          val subVect = internalBasis(lz)
          val c1 = v(lz)
          val c2 = subVect(lz)

          stripVector(i, v - c1 / c2 * subVect, coeff + c1 / c2 * basisCreation(lz))
        }
      }
    }

    vects.zipWithIndex.foreach {
      case (v, i) =>
        val (basisVect, linComb) = stripVector(i, v, DenseVector.zeros[Double](m))
        if (basisVect) {
          basis :+= v
          transposeMatrix(::, i) := oneVect(basis.size - 1, m)
        } else {
          transposeMatrix(::, i) := linComb
        }
    }

    (basis, transposeMatrix(basis.indices, ::))
  }

  /**
   * Sets the boundings of the new variables.
   *
   * @param id (newly created) Identifier whose boundings shall be calculated
   * @param v  Corresponding vector from transposition matrix (indicates how old basis variables are translated into id)
   * @param oldBasis The identifiers from the old basis
   * @param env Corresponding z3 Environment
   */
  private def adjustMinMax(id: Identifier, v: DenseVector[Double], oldBasis: Seq[Identifier], env: Z3QueryEnvironment): Unit = {
    val f = Z3LinAlgFormula(oldBasis.zipWithIndex.map {case (id, idx) => (id, v(idx))}.toMap, 0, env)
    val bounds  = env.boundQuery(f)
    Logging.log(s"Bound adjustment for $id resulted in $bounds", channel = Logging.DEBUG)
    env.addRange(id, bounds._1, bounds._2)
  }

}
