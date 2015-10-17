package fpinscala.chapter03

import org.scalatest.{FunSpec, Matchers}

class Exercise32Test extends FunSpec with Matchers {

  describe("3.2 Tail function") {

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

  describe("3.3 Set head") {

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

  describe("3.4 Drop") {

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

  describe("3.5 Drop while") {
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

    it("should work 2") {
      val l = Exercise32.List(1,2,3)
      Exercise32.List.dropWhile[Int](l, _ % 2 == 0) shouldBe Exercise32.List(1,2,3)
    }
  }

  describe("3.6 Init") {
    it("should drop last") {
      val l = Exercise32.List(1,2,3)
      Exercise32.List.init(l) shouldBe Exercise32.List(1,2)
    }

    it("Nil should return Nil for single element") {
      Exercise32.List.init(Exercise32.List(1)) shouldBe Exercise32.Nil
    }

    it("should throw an exception on empty list") {
      intercept[java.lang.UnsupportedOperationException] {
        Exercise32.List.init(Exercise32.Nil)
      }
    }
  }

  describe("3.9 Length") {

    it("should calculate length") {
      val l = Exercise32.List(1,2,3)
      Exercise32.List.length(l) shouldBe 3
    }

    it("should calculate 0 length") {
      val l = Exercise32.List()
      Exercise32.List.length(l) shouldBe 0
    }
  }

  describe("3.10 foldLeft") {
    it("should foldLeft") {
      val l = Exercise32.List(1,2,3)
      Exercise32.List.foldLeft(l, 2)(_ + _) shouldBe 8
    }
    it("should foldLeft on empty list") {
      val l = Exercise32.List[Int]()
      Exercise32.List.foldLeft(l, 2)(_ + _) shouldBe 2
    }
  }

  describe("3.11 sum and product with foldleft") {
    it("should sum") {
      val l = Exercise32.List(2, 3)
      Exercise32.List.sumLeft(l) shouldBe 5
    }
    it("should do product") {
      val l = Exercise32.List(2.0, 3.0)
      Exercise32.List.productLeft(l) shouldBe 6
    }
  }

  describe("3.12 reverse") {
    it ("should reverse the list") {
      val l = Exercise32.List(1, 2, 3)
      Exercise32.List.reverse(l) shouldBe Exercise32.List(3, 2, 1)
    }
  }
  describe("contains") {
    it ("should find sublist") {
      val l = Exercise32.List(1, 2, 3, 4, 5)
      Exercise32.List.hasSubsequence(l, Exercise32.List(2, 3)) shouldBe true
    }

    it ("should not allow broken sublists") {
      val l = Exercise32.List(1, 2, 3, 4, 5)
      Exercise32.List.hasSubsequence(l, Exercise32.List(2, 4)) shouldBe false
    }
  }
}
