package fpinscala

import fpinscala.Chapter4._
import org.scalatest.{FunSpec, Matchers}

import scala.Predef.augmentString

import Chapter5._
class Chapter5Test extends FunSpec with Matchers {

  val testStream = Cons(() => 10, () => Cons(() => 11, () => Cons(() => 12, () => Empty)))

  describe("toList()") {

    it("convert an empty stream to an empty list") {
      Chapter5.Empty.toList shouldBe Nil
    }

    it("convert an non-empty stream to a non-empty list") {
      testStream.toList shouldEqual List(10, 11, 12)
    }

  }

  describe("take(), drop(), takeWhile()") {
    it("should work for take(0)") {
      testStream.take(0) shouldBe Chapter5.Empty
    }

    it("should work for take(1)") {
      testStream.take(1).toList shouldBe List(10)
    }

    it("should work for take(2)") {
      testStream.take(2).toList shouldBe List(10, 11)
    }

    it("should work for drop(0)") {
      testStream.drop(0) shouldBe testStream
    }

    it("should work for drop(1)") {
      testStream.drop(1).toList shouldBe List(11, 12)
    }

    it("should work for drop(2)") {
      testStream.drop(2).toList shouldBe List(12)
    }

    it("should for takeWhile(n > 100)") {
      testStream.takeWhile(_ > 100) shouldBe Empty
    }

    it("should for takeWhile(n <= 11)") {
      testStream.takeWhile(_ <= 11).toList shouldBe List(10, 11)
    }
  }

}
