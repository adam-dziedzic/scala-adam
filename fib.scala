def fib(n: Int): Int = {
    //@annotation.tailrec
    def go(n: Int): (Int, Int) = {
        print("in go")
	println(n)
    	if (n <= 1) (0,0)
    	else if (n == 2) (1,0)
	else {
	     val(a,b) = go(n-1)
	     (a+b,a)
	}
    }
    println("in fib")
    val(a,b) = go(n)
    a
}

def fib2(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, cur: Int, prev: Int) : Int = {
    	if (n <= 2) cur
	else go(n-1,cur+prev,cur)
    }
    go(n, 1, 0)
}

/* 

5 1 0
4 1 1
3 2 1
2 3 2
1 3

fibonacci numbers:
1 - 0
2 - 1
3 - 1
4 - 2
5 - 3
6 - 5
7 - 8
8 - 13


fib(5) => 3

go(2, 0, 0)

go(1, 0, 0) -> 0 
go(2, 1, 0) -> 1
go(3, 1, 1)
go(4, 2, 1)
go(5, 3, 2)

*/