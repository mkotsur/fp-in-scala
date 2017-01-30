package fpinscala

import fpinscala.Chapter5.Stream.cons

import scala.annotation.tailrec

object Chapter5 {

  sealed trait Stream[+A] {

    // This has been reimplemented using foldRight()
    def headOption_ : Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    // foldRight() - based implementation
    def headOption: Option[A] = this.foldRight[Option[A]](None)((a, _) => Some(a))

    // Exercice 5.1
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() +: t().toList
    }

    def take(n: Int): Stream[A] = this match {
      case _ if n == 0 => Empty
      case Cons(h, _) if n == 1 => Cons(h, () => Empty)
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
      case Empty => throw new IllegalStateException("Can't take elements from empty stream")
    }

    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case _ if n == 0 => this
      case Empty => throw new IllegalStateException("Can't drop elements on empty stream")
      case Cons(_, t) => t().drop(n - 1)
    }

    // This has been reimplemented using foldRight()
    def takeWhile_(p: A => Boolean): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) => {
        if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
      }
    }

    // foldRight() - based implementation
    def takeWhile(p: A => Boolean): Stream[A] = this.foldRight[Stream[A]](Empty)((a, b) => {
      if (p(a)) Cons(() => a, () => b.takeWhile(p))
      else b
    })

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((h, t) => cons[B](f(h), t))

    def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) =>
      if (f(h))
        cons(h, t)
      else
        t
    )

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((h, t) => f(h).append(t))

    def append[B >: A](s: => Stream[B]): Stream[B] = this match {
      case Empty => s
      case Cons(h, t) => cons[B](h(), t().append(s))
    }

  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {

    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    def from(n: Int): Stream[Int] = cons(n, from(n+1))

    def fibs(k: Int = 0, n: Int = 1): Stream[Int] =
      cons(k, fibs(n, k + n))

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
