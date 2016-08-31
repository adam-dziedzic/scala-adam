def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
    	if (as.length <= n) true
	else if (!(ordered(as(n-1),as(n)))) false
	else loop(n+1)
    }
    println("array length: " + as.length)
    loop(1)
}

isSorted(Array(1,2,3,4), (x: Int, y: Int) => x <= y)

isSorted(Array(1,3,2), (x: Int, y: Int) => x <= y)
