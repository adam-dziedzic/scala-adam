/* partial application of a function - we want to apply function f but know we 
give only the argument a but later on we will also provide b */
def partial1[A,B,C](a: A, f: (A,B) => C): B=>C = {
    (b : B) =>  f(a, b)
}

def sum(a : Int, b : Int) : Int = {
    a + b
}

def run() {
    val bfun = partial1(1, sum)
    val result = bfun(2)
    println("result: " + result)
}