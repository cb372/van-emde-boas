package veb

sealed trait Tree {
  import Tree._

  /**
   * The "universe" size of this van Embe Boas tree
   */
  val u: Int

  protected var min: Int
  protected var max: Int

  /**
   * Check whether the vEB tree contains the given value.
   */
  def contains(x: Int): Boolean = {
    if (x == min || x == max)
      true
    else this match {
      case _: VebTwo =>
        false
      case Veb(_, _, _, _, cluster) =>
        cluster(high(x, u)).contains(low(x, u))
    }
  }

  /**
   * Return the minimum element in the tree, if there is one.
   */
  def minimum: Option[Int] = Some(min).filterNot(_ == NIL)

  /**
   * Return the maximum element in the tree, if there is one.
   */
  def maximum: Option[Int] = Some(max).filterNot(_ == NIL)

  /**
   * Insert the given value.
   * Does nothing if the value is already in the tree,
   * or is not in the correct range.
   */
  def insert(x: Int): Unit = {
    if (!contains(x) && x >= 0 && x < u) {
      unsafeInsert(x)
    }
  }

  /**
   * Insert the given value.
   * Assumes that the already is not already in the tree,
   * and is in the correct range.
   */
  def unsafeInsert(x: Int): Unit = {
    if (min == NIL) {
      insertIntoEmpty(x)
    } else {
      var target = x

      if (x < min) {
        target = min
        min = x
      }

      this match {
        case _: VebTwo =>
          // nothing to do
        case Veb(_, _, _, summary, cluster) =>
          val h = high(target, u)
          val l = low(target, u)
          if (cluster(h).min == NIL) {
            summary.unsafeInsert(h)
            cluster(h).insertIntoEmpty(l)
          } else {
            cluster(h).unsafeInsert(l)
          }
      }

      if (target > max) {
        max = x
      }
    }
  }

  private def insertIntoEmpty(x: Int): Unit = {
    min = x
    max = x
  }

  /**
   * Delete the given value.
   * Does nothing if the value is not in the tree.
   */
  def delete(x: Int): Unit = {
    if (contains(x)) {
      unsafeDelete(x)
    }
  }

  /**
   * Delete the given value.
   * Assumes that the value is in the tree.
   */
  def unsafeDelete(x: Int): Unit = {
    if (min == max) {
      min = NIL
      max = NIL
    } else this match {
      case _: VebTwo =>
        if (x == 0) {
          min = 1
        } else {
          min = 0
        }
        max = min
      case Veb(_, _, _, summary, cluster) =>
        var target = x
        if (x == min) {
          val firstCluster = summary.min
          target = index(firstCluster, cluster(firstCluster).min, u)
          min = target
        }
        val h = high(target, u)
        val l = low(target, u)
        cluster(h).unsafeDelete(l)
        if (cluster(h).min == NIL) {
          summary.unsafeDelete(h)
          if (target == max) {
            summary.maximum match {
              case Some(summaryMax) =>
                max = index(summaryMax, cluster(summaryMax).max, u)
              case None =>
                max = min
            }
          }
        } else {
          if (target == max) {
            max = index(h, cluster(h).max, u)
          }
        }
    }
  }

  /**
   * Find the first element in the tree that is larger
   * than the given value.
   *
   * The given value does not have to be in the tree,
   * but it is assumed to be in the correct range.
   */
  def successor(x: Int): Option[Int] = this match {
    case _: VebTwo =>
      if (x == 0 && max == 1)
        Some(1)
      else
        None
    case Veb(_, _, _, summary, cluster) =>
      if (min != NIL && x < min)
        Some(min)
      else {
        val h = high(x, u)
        val l = low(x, u)
        cluster(h).maximum match {
          case Some(maxLow) if l < maxLow =>
            cluster(h).successor(l)
              .map(offset => index(h, offset, u))
          case _ =>
            for {
              succCluster <- summary.successor(h)
              offset <- cluster(succCluster).minimum
            } yield index(succCluster, offset, u)
        }
      }
  }

  /**
   * Find the last element in the tree that is smaller
   * than the given value.
   *
   * The given value does not have to be in the tree,
   * but it is assumed to be in the correct range.
   */
  def predecessor(x: Int): Option[Int] = this match {
    case _: VebTwo =>
      if (x == 1 && min == 0)
        Some(0)
      else
        None
    case Veb(_, _, _, summary, cluster) =>
      if (max != NIL && x > max)
        Some(max)
      else {
        val h = high(x, u)
        val l = low(x, u)
        cluster(h).minimum match {
          case Some(minLow) if l > minLow =>
            cluster(h).predecessor(l)
              .map(offset => index(h, offset, u))
          case _ =>
            summary.predecessor(h) match {
              case Some(predCluster) =>
                cluster(predCluster).maximum
                  .map(offset => index(predCluster, offset, u))
              case None =>
                if (min != NIL && x > min)
                  Some(min)
                else
                  None
            }
        }
      }
  }

}

/**
 * A vEB tree with a universe size > 2
 */
private[veb] case class Veb(val u: Int, var min: Int, var max: Int, summary: Tree, cluster: Array[Tree]) extends Tree {
  override def toString: String =
    s"vEB tree(u = $u, min = $min, max = $max, summary = $summary, cluster = ${cluster.map(_.toString). mkString("[", ", ", "]")}"
}

/**
 * A two-element vEB tree
 */
private[veb] case class VebTwo(var min: Int, var max: Int) extends Tree {
  val u = 2
  override def toString: String =
    s"2-element vEB tree(min = $min, max = $max)"
}

object Tree {

  private val NIL = -1

  /**
   * Create an empty vEB tree with at least the specified capacity.
   *
   * The maximum supported capacity is 2^30.
   */
  def create(capacity: Int): Tree = {
    def roundUpToNextPowerOfTwo(x: Int): Int = {
      val k = 32 - Integer.numberOfLeadingZeros(x - 1)
      Math.ceil(Math.pow(2, k)).toInt
    }

    val u = {
      if (capacity <= 2)
        2
      else if (capacity > 1073741824)
        throw new IllegalArgumentException("Cannot create a vEB tree larger than 2^30")
      else
        roundUpToNextPowerOfTwo(capacity)
    }

    buildEmptyTree(u)
  }

  private def buildEmptyTree(u: Int): Tree = u match {
    case 2 =>
      VebTwo(min = NIL, max = NIL)
    case _ => // more than 2
      val upper = upperRoot(u)
      val lower = lowerRoot(u)
      val summary = buildEmptyTree(upper)
      val cluster = Array.fill(upper)(buildEmptyTree(lower))
      Veb(u, min = NIL, max = NIL, summary, cluster)
  }

  private def lowerRoot(u: Int): Int =
    Math.pow(2, Math.floor(log2(u).toDouble / 2)).toInt

  private def upperRoot(u: Int): Int =
    Math.pow(2, Math.ceil(log2(u).toDouble / 2)).toInt

  private def high(x: Int, u: Int): Int =
    Math.floor(x / lowerRoot(u)).toInt

  private def low(x: Int, u: Int): Int =
    x % lowerRoot(u)

  private def index(x: Int, y: Int, u: Int): Int =
    x * lowerRoot(u) + y

  private def log2(n: Int): Int =
    31 - Integer.numberOfLeadingZeros(n)

}
