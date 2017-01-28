package fpinscala

import scala.annotation.tailrec

object Chapter5 {

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    // Exercice 5.1
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() +: t().toList
    }

    def take(n: Int): Stream[A] = this match {
      case _ if n == 0 => Empty
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
      case Empty => throw new IllegalStateException("Can't take elements from empty stream")
    }

    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case _ if n == 0 => this
      case Empty => throw new IllegalStateException("Can't drop elements on empty stream")
      case Cons(_, t) => t().drop(n - 1)
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) => {
        if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
      }
    }
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else Stream.cons(as.head, apply(as.tail: _*))
}
