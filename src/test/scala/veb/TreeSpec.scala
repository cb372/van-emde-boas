package veb

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._

class TreeSpec extends PropSpec with GeneratorDrivenPropertyChecks {

  override implicit val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 500)

  implicit def doNotShrink[A]: Shrink[A] = Shrink[A](_ => Stream.empty)

  private val uGen: Gen[Int] =
    for {
      k  <- Gen.choose(0, 10)
      u  <- Gen.const(Math.pow(2, k).toInt)
    } yield u

  property("tree capacity is always at least as large as requested") {
    forAll(Gen.choose(0, 10000000), minSuccessful(100)){ capacity: Int =>
      assert(Tree.create(capacity).u >= capacity)
    }
  }

  property("empty tree contains no elements") {
    forAll(uGen) { u =>
      val tree = Tree.create(u)

      for (i <- 0 until u) {
        assert(!tree.contains(i))
      }
    }
  }

  property("empty tree has no minimum or maximum") {
    forAll(uGen) { u =>
      val tree = Tree.create(u)

      assert(tree.minimum == None)
      assert(tree.maximum == None)
    }
  }

  property("tree contains all inserted elements and nothing else") {
    val gen =
      for {
        u  <- uGen
        xs <- Gen.listOf(Gen.choose(0, u - 1))
      } yield (u, xs)

    forAll(gen) { case (u, xs) =>
      val tree = Tree.create(u)

      xs.foreach(tree.insert)

      for (i <- 0 until u) {
        if (xs.contains(i))
          assert(tree.contains(i))
        else
          assert(!tree.contains(i))
      }
    }
  }

  property("tree contains all inserted elements apart from subsequently deleted ones") {
    val gen =
      for {
        u       <- uGen
        inserts <- Gen.listOf(Gen.choose(0, u - 1))
        deletes <- Gen.someOf(inserts)
      } yield (u, inserts, deletes)

    forAll(gen) { case (u, inserts, deletes) =>
      val tree = Tree.create(u)

      inserts.foreach(tree.insert)
      deletes.foreach(tree.delete)

      for (i <- 0 until u) {
        if (inserts.contains(i) && !deletes.contains(i))
          assert(tree.contains(i))
        else
          assert(!tree.contains(i))
      }
    }
  }

  property("minimum and maximum") {
    val gen =
      for {
        u  <- uGen
        xs <- Gen.listOf(Gen.choose(0, u - 1))
      } yield (u, xs)

    forAll(gen) { case (u, xs) =>
      val tree = Tree.create(u)

      xs.foreach(tree.insert)

      if (xs.isEmpty) {
        assert(tree.minimum == None)
        assert(tree.maximum == None)
      } else {
        assert(tree.minimum == Some(xs.min))
        assert(tree.maximum == Some(xs.max))
      }
    }
  }

  property("successor") {
    val gen =
      for {
        u  <- uGen
        xs <- Gen.listOf(Gen.choose(0, u - 1))
      } yield (u, xs)

    forAll(gen) { case (u, xs) =>
      val tree = Tree.create(u)

      xs.foreach(tree.insert)

      val sorted = xs.sorted
      for (i <- 0 until u) {
        val expected = sorted.find(_ > i)
        assert(tree.successor(i) == expected)
      }
    }
  }

  property("predecessor") {
    val gen =
      for {
        u  <- uGen
        xs <- Gen.listOf(Gen.choose(0, u - 1))
      } yield (u, xs)

    forAll(gen) { case (u, xs) =>
      val tree = Tree.create(u)

      xs.foreach(tree.insert)

      val sorted = xs.sorted.reverse
      for (i <- 0 until u) {
        val expected = sorted.find(_ < i)
        assert(tree.predecessor(i) == expected)
      }
    }
  }

  property("minimum, maximum, predecessor and successor after deletions") {
    val gen =
      for {
        u       <- uGen
        inserts <- Gen.listOf(Gen.choose(0, u - 1))
        deletes <- Gen.someOf(inserts)
      } yield (u, inserts, deletes)

    forAll(gen) { case (u, inserts, deletes) =>
      val tree = Tree.create(u)

      inserts.foreach(tree.insert)
      deletes.foreach(tree.delete)

      val xs = (inserts.toSet -- deletes.toSet).toList

      if (xs.isEmpty) {
        assert(tree.minimum == None)
        assert(tree.maximum == None)
      } else {
        assert(tree.minimum == Some(xs.min))
        assert(tree.maximum == Some(xs.max))
      }

      val sorted = xs.sorted
      for (i <- 0 until u) {
        val expected = sorted.find(_ > i)
        assert(tree.successor(i) == expected)
      }

      val reverseSorted = sorted.reverse
      for (i <- 0 until u) {
        val expected = reverseSorted.find(_ < i)
        assert(tree.predecessor(i) == expected)
      }
    }
  }


}

