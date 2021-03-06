package fpinscala.chapter03

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Exercise32 {

  sealed trait List[+A] // `List` data type, parameterized on a type, `A`
  case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
  /* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
  which may be `Nil` or another `Cons`.
   */
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = // Variadic function syntax
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
      }

    def sum2(ns: List[Int]) =
      foldRight(ns, 0)((x,y) => x + y)

    def sumLeft(ns: List[Int]) =
      foldLeft(ns, 0)((x,y) => x + y)

    def product2(ns: List[Double]) =
      foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

    def productLeft(ns: List[Double]) =
      foldLeft(ns, 1.0)(_ * _)


    def tail[A](l: List[A]): List[A] = l match {
      case Cons(_, t) => t
      case _ => throw new UnsupportedOperationException("Tail is not supported")
    }

    def setHead[A](l: List[A], h: A): List[A] = l match {
      case Nil => throw new UnsupportedOperationException("setHead is not supported")
      case _ => Cons(h, tail(l))
    }

    def drop[A](l: List[A], n: Int): List[A] = {
      (1 to n).foldLeft(l) { (l, _) => l match {
        case Nil => throw new IllegalArgumentException(s"Can not drop $n elements")
        case _ => tail(l)
      }
      }
    }

    // NOTE: it seems not to be necessary to group stuff into 2 argument lists anymore
    @tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => throw new UnsupportedOperationException("Can't do init on empty list")
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }



    def length[A](l: List[A]): Int = foldRight(l, 0)( (_, v) => v + 1)

    @tailrec
    def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }


    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def hasSubsequence[A](l: List[A], sl: List[A]): Boolean =
      foldLeft(l, (false, sl))(
        (acc, next) => {
          val (works, rest) = acc
          rest match {
            case Nil => (works, Nil)
            case Cons(x, xs) if x == next =>
              (true, xs)
            case Cons(x, xs) =>
              (false , sl)
          }
        })._1


    def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")

    def reverse[A](l: List[A]): List[A] = ???
  }


}
