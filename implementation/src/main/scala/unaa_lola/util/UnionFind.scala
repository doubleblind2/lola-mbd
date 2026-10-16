package unaa_lola.util

import scala.collection.mutable.{HashMap, Map}

/*
 *  Implements a standard Union-Find (a.k.a Disjoint Set) data
 *  structure with permissive behavior with respect to
 *  non-existing elements in the structure (Unknown elements are
 *  added as new elements when queried for).
 *
 * See Cormen, Thomas H.; Leiserson, Charles E.; Rivest, Ronald
 * L.; Stein, Clifford (2001), "Chapter 21: Data structures for
 * Disjoint Sets", Introduction to Algorithms (Second ed.), MIT
 * Press, pp. 498–524, ISBN 0-262-03293-7
 *
 * Amortized time for a sequence of m {union, find} operations
 * is O(m * InvAckermann(n)) where n is the number of elements
 * and InvAckermann is the inverse of the Ackermann function.
 *
 * Not thread-safe.
 *
 * Taken from: from https://github.com/scala-ide/scala-refactoring/blob/master/src/main/scala/scala/tools/refactoring/util/UnionFind.scala
 * as part of the Scala Refactoring project on GitHub.
 *
 * License: Scala license
 */

/**
 * Union-Find data structure
 * Taken from: from https://github.com/scala-ide/scala-refactoring/blob/master/src/main/scala/scala/tools/refactoring/util/UnionFind.scala
 * @tparam T Type of elements in structure
 */
class UnionFind[T]() {

  private val parent: Map[T, T] = new HashMap[T,T] {
    override def default(s: T) = {
      get(s) match {
        case Some(v) => v
        case None    => put(s, s); s
      }
    }
  }

  private val rank: Map[T, Int] = new HashMap[T,Int] {
    override def default(s: T) = {
      get(s) match {
        case Some(v) => v
        case None    => put(s, 1); 1
      }
    }
  }

  /**
    * Return the parent (representant) of the equivalence class.
    * Uses path compression.
    */
  def find(s: T): T = {
    val ps = parent(s)
    if (ps == s) s else {
      val cs = find(ps)
      parent(s) = cs
      cs
    }
  }

  /**
    *  Unify equivalence classes of elements.
    *  Uses union by rank.
    */
  def union(x: T, y: T): Unit = {
    val cx = find(x)
    val cy = find(y)
    if (cx != cy) {
      val rx = rank(x)
      val ry = rank(y)
      if (rx > ry) parent(cy) = cx
      else if (rx < ry) parent(cx) = cy
      else {
        rank(cx) += 1
        parent(cy) = cx
      }
    }
  }

  /**
    * Enumerates the equivalence class of element x
    */
  def equivalenceClass(x: T): List[T] = {
    val px = find(x)
    parent.keys.filter(find(_:T) == px).toList
  }

  override def toString : String = {
    "UnionFind with parent Map: \n" + parent.toString
  }

  /**
   * Create map from UF structure
   */
  def toMap : scala.collection.immutable.Map[T, T] = {
    parent.map{case (k, v) => (k, find(v))}.toMap
  }

  /**
   * Create map (as sequence) from UF structure
   */
  def toSeqMap : scala.collection.immutable.Map[T, Seq[T]] = {
    toMap.groupBy(_._2).view.mapValues(_.keys.toSeq).toMap
  }

}