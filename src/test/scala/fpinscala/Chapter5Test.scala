package fpinscala

import org.scalatest.{FunSpec, Matchers}


import Chapter5._
class Chapter5Test extends FunSpec with Matchers {

  val testStream = Cons(() => 10, () => Cons(() => 11, () => Cons(() => 12, () => Empty)))

  describe("headOption()") {
    it("should return None on Empty") {
      Empty.headOption shouldBe None
    }

    it("should return Some on non-Empty") {
      testStream.headOption shouldBe Some(10)
    }
  }

  describe("toList()") {

    it("convert an empty stream to an empty list") {
      Chapter5.Empty.toList shouldBe Nil
    }

    it("convert an non-empty stream to a non-empty list") {
      testStream.toList shouldEqual List(10, 11, 12)
    }

  }

  describe("Exercise 5.2, 5.2: take(), drop(), takeWhile()") {
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

  describe("Exercise 5.4: forAll()") {
    it("should detect that all elements satisfy the condition") {
      testStream.forAll(_ > 0) shouldBe true
    }
    it("should detect that some elements don't satisfy the condition") {
      testStream.forAll(_ > 11) shouldBe false
    }
    it("should not evaluate all the elements if the condition is broken half-way") {
      var executed = false
      val lastElement = () => {
        executed = true
        14
      }
      Cons(() => 12, () => Cons(() => 13, () => Cons(lastElement, () => Empty)))
        .forAll(_ < 13) shouldBe false

      executed shouldBe false
    }
  }

  describe("Exercise 5.7: map, filter, flatMap") {
    it("map should not evaluate elements when not needed") {
      var executed = false
      val mappedStream = testStream.map { i =>
        if (i > 11) {
          executed = true
        }
        i * 2
      }

      mappedStream.take(1).toList shouldBe List(20)
      executed shouldBe false
    }

    it("filter should not evaluate elements when not needed") {
      var executed = false
      val mappedStream = testStream.filter { i =>
        if (i > 11) executed = true
        i <= 10
      }

      mappedStream.take(1).toList shouldBe List(10)
      executed shouldBe false
    }

    it("flatMap should work") {
      var executed = false
      val mappedStream = testStream.flatMap { i =>
        if (i > 10) executed = true
        testStream.map(_ + i)
      }

      mappedStream.take(1).toList shouldBe List(20)
      executed shouldBe false
    }

    it("append should work") {
      testStream.append(testStream).toList shouldBe List(10, 11, 12, 10, 11, 12)
    }

    it("append should be lazy") {
      var executed = false
      testStream.append({
        executed = true
        testStream
      }).take(1).toList shouldBe List(10)

      executed shouldBe false
    }

  }

}
