package fpinscala

import fpinscala.Chapter4._
import org.scalatest.{FunSpec, Matchers}

import scala.Predef.augmentString

class Chapter4Test extends FunSpec with Matchers {

  describe("map2") {
    it("should combine 2 options") {
      val mapper = (a: String, b: Int) => a + b.toString

      map2(None, None)(mapper) shouldBe None
      map2(None, Some(2))(mapper) shouldBe None
      map2(Some("2"), Some(2))(mapper) shouldBe Some("22")
    }
  }

  describe("Variance") {

    it("should return None for an empty sequence") {
      variance(Nil) shouldBe None
    }

    it("should return variance 0 for a non-empty sequence") {
      variance(List(2, 2)) shouldBe Some(0)
    }

    it("should return variance non-0 for a non-empty sequence") {
      variance(List(2, 4)) shouldBe Some(1)
    }
  }

  describe("Sequence") {

    it("Should work for a single value list") {
      sequence(List(Some(1))) shouldBe Some(List(1))
    }

    it("Should combine values of options") {
      sequence(List(Some(1), Some(2))) shouldBe Some(List(1, 2))
    }

    it("Should return None if at least one is None") {
      sequence(List(Some(1), None)) shouldBe None
    }

  }

  describe("traverse") {

    it("should traverse an empty list") {
      traverse(Nil)(v => None) shouldBe Some(Nil)
    }

    it("should traverse a non-empty list") {
      traverse(List("1", "2"))(v => Some(v.toInt)) shouldBe Some(List(1, 2))
    }

  }
}
