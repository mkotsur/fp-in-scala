package fpinscala.chapter03

object Exercise31 extends App {


  val x1 = List(1,2,3,4,5) match {
    case x :: 2 :: 4 :: _ => x
    case Nil => 42
    case x :: y :: 3 :: 4 :: _ => x + y
    case h :: t => h + t.sum
    case _ => 101
  }



  println(x1)
}
