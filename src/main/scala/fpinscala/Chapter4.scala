package fpinscala

object Chapter4 {

  def variance(xs: Seq[Double]): Option[Double] = {
    def mean (xxs: Seq[Double]) = if (xxs.isEmpty) None
    else Some(xxs.sum / xxs.length)

    val f =  (x: Int) => x * 15

    mean(xs) flatMap(v => mean(xs.map(x => Math.pow(x - v, 2))))
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(aa => b map ( bb => f(aa, bb) ) )

  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => map2(f(x), traverse(xs)(f))( (v, l) => v +: l)
  }


}
