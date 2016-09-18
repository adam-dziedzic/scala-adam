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

       def tail[A](list: List[A]): List[A] = list match {
           case Nil => sys.error("Empty list")
	   case Cons(_, xs) => xs
       }

       def addHead[A](head: A, list: List[A]): List[A] = list match {
       	   case Nil => Cons(head, Nil)
	   case Cons(x,xs) => Cons(head, Cons(x,xs))
       }

       def setHead[A](head: A, list: List[A]): List[A] = list match {
       	   case Nil => Cons(head, Nil)
	   case Cons(_,xs) => Cons(head,xs)
       }

       def drop[A](l: List[A], n: Int): List[A] = n match {
       	   case 0 => l
	   case _ => l match {
	   	case Nil => sys.error("No elements in the list")
		case Cons(_,xs) => drop(xs, n-1)
	   }
       }

       def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
       	   case Nil => Nil
	   case Cons(x,xs) => {
	   	if (f(x)) dropWhile(xs, f) 
		else l
	   }        	   
       }

       def init[A](l: List[A]): List[A] = l match {
       	   case Nil => Nil
	   case Cons(_, Nil) => Nil
	   case Cons(x, xs) => Cons(x,init(xs))
       }

       def append[A](a: A, l: List[A]): List[A] = {
       	   foldRight(l, a)(Cons(_,_))
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


def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
    println(as)
    as match {
       case Nil => z
       case Cons(x,xs) => f(x, foldRight(xs,z)(f))
    }
}

def product3(xs: List[Double]) =
    foldRight(xs, 1.0)(_ * _)

/* Fold right 2 with a zeor element - which zeros multiplication */
def foldRight2[A](as: List[A], z: A, zero: A)(f: (A,A) => A): A = {
    println(as)
    if (zero != Nil) {
       println("in zero")
       as match {
       	  case Nil => { 
	       println("encountered nil")
	       return z
	  }
	  case Cons(x, _) => { 
	       if (x == zero) {
	       	  println("first element is zero: " + zero)
      	       	  return zero
	       }
	  }
       }
    }
    println(as)
    as match {
       case Nil => z
       case Cons(x,xs) => {
            f(x, foldRight2(xs, z, zero)(f))
       }
    }
}

def foldRight3[A](as: List[A], z: A, zero: A)(f: (A,A) => A): A = {
    println(as)
    as match {
       case Nil => z
       case Cons(zero, xs) => zero
       case Cons(x,xs) => f(x, foldRight3(xs,z,zero)(f))
    }
}

def product4(xs: List[Double]) =
    foldRight2(xs, 1.0, 0.0)(_ * _)

def product5(xs: List[Double]) =
    foldRight3(xs, 1.0, 0.0)(_ * _)

def foldRight4[A,B](as: List[A], z: B, zero: B)(f: (A,B) => B): B = {
    println(as)
    if (zero != Nil) {
       println("in zero")
       as match {
       	  case Nil => { 
	       println("encountered nil")
	       return z
	  }
	  case Cons(x, _) => { 
	       if (x == zero) {
	       	  println("first element is zero: " + zero)
      	       	  return zero
	       }
	  }
       }
    }
    println(as)
    as match {
       case Nil => z
       case Cons(x,xs) => {
            f(x, foldRight4(xs, z, zero)(f))
       }
    }
}

def product6(xs: List[Double]) =
    foldRight4(xs, 1.0, 0.0)(_ * _)

def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_,b)=>1+b)
}

def foldRight10[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
    as match {
       case Nil => z
       case Cons(x, xs) => f(x, foldRight10(xs,z)(f))
    }
}

def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    println("sum left as:"+as)
    as match {
       case Nil => z
       case Cons(x,xs) => f(foldLeft(xs,z)(f), x)
    }
}

def foldLeft2[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    @annotation.tailrec
    def go[A,B](as: List[A], acc: B)(f: (B,A) => B): B = {
        println("sum left as:"+as)
    	as match {
	   case Nil => acc
	   case Cons(x,xs) => go(xs, f(acc,x))(f)
	}
    }
    go(as,z)(f)
}

@annotation.tailrec
def foldLeft3[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    as match {
       case Nil => z
       case Cons(x,xs) => foldLeft3(xs, f(z,x))(f)
    }
}

def sumLeft(ns: List[Int]) = 
    foldLeft3(ns,0)((x,y)=>x+y)

def productLeft(ns: List[Double]) = {
    foldLeft3(ns,1.0)(_ * _)    
}

def lengthLeft[A](ns: List[A]) = {
    foldLeft(ns,0)((b,_)=>b+1)
}

def reverse[A](ns: List[A]) = {
    foldLeft3(ns,Nil:List[A])((b,a)=>Cons(a,b))
}

// def reverse2[A](ns: List[A]) = {
//      foldRight10(ns,Nil:List[A])((a,b)=>())
// }

def foldRight11[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
    foldLeft3(as,z)(reverseArgs(f))
}

def reverseArgs[A,B](f: (A,B) => B): (B,A) => B = {
    (b: B, a: A) => f(a,b)
}

def printRevers[A,B](a: A, b: B) {
    println(a)
    println(b)
}

// def listMe[A](a : A, b : A): List[A] = {
//     List(a,b)
// }

