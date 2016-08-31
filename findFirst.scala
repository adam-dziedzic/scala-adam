def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = 
    	if (n >= as.length) -1
	else if (p(as(n))) n
	else loop(n+1)
    loop(0) // start the loop at the first element of the array
}

def equal[A](a: A): Boolean = {
    if (a == 2) true
    else false
}

println("find first")
val array = Array(1,3,5,2,10)
println(findFirst(array, equal))

