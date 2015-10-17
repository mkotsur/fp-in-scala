package fpinscala.chapter03

import fpinscala.chapter03.Exercise325.MaxableThings.Maxable

object Exercise325 extends App {

  object MaxableThings {

    trait Maxable[T] {
      def myMax(a: T, b: T) : T
    }

    implicit object MaxableInt extends Maxable[Int] {
      def myMax(a: Int, b: Int) : Int = a.max(b)
    }

    implicit object MaxableDouble extends Maxable[Double] {
      def myMax(a: Double, b: Double) : Double = a.max(b)
    }

  }

  sealed trait Tree[+A] {

    def size: Int

    def maximum: A

    def depth: Int

    def map[B](f: A => B)(implicit ev: Maxable[B]): Tree[B]

    def fold[A1 >: A](init: A1)(f: (A1, A1) => A1)(implicit ev: Maxable[A1]): A1

  }

  case class Leaf[A](value: A) extends Tree[A] {
    override def size: Int = 1

    override def maximum: A = value

    override def depth: Int = 0

    override def map[B](f: (A) => B)(implicit ev: Maxable[B]): Tree[B] = Leaf(f(value))

    override def fold[A1 >: A](init: A1)(f: (A1, A1) => A1)(implicit ev: Maxable[A1]): A1 = f(init, value)
  }

  case class Branch[A](left: Tree[A], right: Tree[A])(implicit ev: Maxable[A]) extends Tree[A] {
    override def size: Int = left.size + right.size

    override def maximum: A = ev.myMax(left.maximum, right.maximum)

    override def depth: Int = 1 + left.depth.max(right.depth)

    override def map[B](f: (A) => B)(implicit ev: Maxable[B]): Tree[B] = {
      Branch(left.map(f), right.map(f))
    }

    override def fold[A1 >: A](init: A1)(f: (A1, A1) => A1)(implicit ev: Maxable[A1]): A1 = right.fold(
      left.fold(init)(f)
    )(f)
  }

}
