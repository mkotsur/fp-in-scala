package fpinscala.chapter03

import org.scalatest.{FunSpec, Matchers}

class Exercise32Test extends FunSpec with Matchers {

  describe("Tail function") {

    it("should work") {
      val l = Exercise32.List(1, 2, 3)
      Exercise32.List.tail(l) shouldBe Exercise32.List(2, 3)
    }

    it("should throw an exception on empty list") {
      intercept[java.lang.UnsupportedOperationException] {
        Exercise32.List.tail(Exercise32.Nil)
      }
    }

    it("should return Nil if single element") {
      val l = Exercise32.List(1)
      Exercise32.List.tail(l) shouldBe Exercise32.Nil
    }

  }

  describe("Set head") {

    it("should set head correctly") {
      val l = Exercise32.List(1, 2, 3)
      Exercise32.List.setHead(l, 0) shouldBe Exercise32.List(0, 2, 3)
    }

    it("should set head correctly for single element") {
      val l = Exercise32.List(1)
      Exercise32.List.setHead(l, 0) shouldBe Exercise32.List(0)
    }

    it("should throw an exception for Nil") {
      val e = intercept[UnsupportedOperationException]{
        Exercise32.List.setHead(Exercise32.Nil, 0)
      }

      e.getMessage should include ("setHead")
    }
  }

  describe("Drop") {

    it("should drop first n elements") {
      val l = Exercise32.List(1, 2, 3)
      Exercise32.List.drop(l, 2) shouldBe Exercise32.List(3)
    }

    it("should drop 0 elements") {
      val l = Exercise32.List(1, 2, 3)
      Exercise32.List.drop(l, 0) shouldBe Exercise32.List(1, 2, 3)
    }

    it("should not work for Nil list") {
      val e = intercept[IllegalArgumentException] {
        Exercise32.List.drop(Exercise32.Nil, 1)
      }

      e.getMessage should include ("drop")
    }

    it("should not drop more elements that are there in the list") {
      val e = intercept[IllegalArgumentException] {
        Exercise32.List.drop(Exercise32.List(1, 2), 3)
      }

      e.getMessage should include ("drop")
    }

  }

  describe("Drop while") {
    it("should drop elements") {
      val l = Exercise32.List("aaa", "abb", "bbb", "aab")
      val dropped = Exercise32.List.dropWhile[String](l, _.startsWith("a"))
      dropped shouldBe Exercise32.List("bbb", "aab")
    }

    it("should return Nil ") {
      val l = Exercise32.Nil
      Exercise32.List.dropWhile[String](l, _.startsWith("a")) shouldBe l

    }
    it("should drop all elements") {
      val l = Exercise32.List(1,2,3)
      Exercise32.List.dropWhile[Int](l, _ < 4) shouldBe Exercise32.Nil
    }
  }

  describe("Init") {
    it("should drop last") {
      val l = Exercise32.List(1,2,3)
      Exercise32.List.init(l) shouldBe Exercise32.List(1,2,3)
    }

    it("Nil should return Nil") {
      Exercise32.List.init(Exercise32.Nil) shouldBe Exercise32.Nil
    }
  }
}
