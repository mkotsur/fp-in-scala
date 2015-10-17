package fpinscala.chapter03

import fpinscala.chapter03.Exercise325.{Branch, Leaf}
import org.scalatest.{FunSpec, Matchers}

class Exercise325Test extends FunSpec with Matchers {

  describe("Our tree") {

    it("should count size of non-empty tree") {
      Branch(
        Leaf(13),
        Branch(Leaf(42), Leaf(43))
      ).size shouldBe 3
    }

    it("should count size of a leaf as 1") {
      Leaf(42).size shouldBe 1
    }

    it("should calculate maximum of a tree") {
      Branch(
        Leaf(13),
        Branch(Leaf(42), Leaf(43))
      ).maximum shouldBe 43
    }
    it("should calculate maximum of a double tree") {
      Branch(
        Leaf(13.0),
        Branch(Leaf(43.1), Leaf(43.5))
      ).maximum shouldBe 43.5
    }

    it("should calculate maximum of a leaf") {
      Leaf(42).maximum shouldBe 42
    }

    it("should calculate depth for a leaf") {
      Leaf(42).depth shouldBe 0
    }

    it("should calculate depth for a tree") {
      Branch(
        Leaf(13),
        Branch(
          Leaf(42),
          Branch(Leaf(17), Leaf(38))
        )
      ).depth shouldBe 3
    }

    it("should map a tree into the same type") {
      val originalTree = Branch(
        Leaf(13),
        Branch(Leaf(42), Leaf(43))
      )

      val incrementedTree = Branch(
        Leaf(14),
        Branch(Leaf(43), Leaf(44))
      )

      originalTree.map(_ + 1) shouldEqual incrementedTree
    }

    it("should map a tree into different type") {
      val originalTree = Branch(
        Leaf(13.5),
        Branch(Leaf(42.1), Leaf(43.7))
      )

      val mappedTree = Branch(
        Leaf(14),
        Branch(Leaf(42), Leaf(44))
      )

      originalTree.map(_.round.toInt) shouldEqual mappedTree
    }

    it("should fold a leaf") {
      Leaf(2).fold(42)(_ + _) shouldBe 44
    }

    it("should fold a tree") {
      val originalTree = Branch(
        Leaf(1),
        Branch(Leaf(2), Leaf(3))
      )

      originalTree.fold(42)(_ + _) shouldBe 48
    }
  }
}
