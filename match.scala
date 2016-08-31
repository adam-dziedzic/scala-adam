sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {
       def sum(ints: List[Int]): Int = ints match {
       	   case Nil => 0
	   case Cons(x,xs) => x + sum(xs)
       }

       def product(ds: List[Double]): Double = ds match {
       	   case Nil => 1.0
	   case Cons(0.0, _) => 0.0
	   case Cons(x,xs) => x * product(xs)
       }

       def apply[A](as: A*): List[A] = {
       	   if (as.isEmpty) Nil
	   else Cons(as.head, apply(as.tail: _*))
       }

       val x = List(1,2,3,4,5) match {
              case Cons(x, Cons(2, Cons(4,_))) => x // won't work, the 3rd one should be Cons(3,_)
       	      case Nil => 42
	      // the result should be 3
	      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
	      case Cons(h,t) => h + sum(t)
	      case _ => 101
       }

       println("value of x: " + x)
}

List(1,2,3)

