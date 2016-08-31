def uncurry[A,B,C] (f: A => B => C): (A,B) => C = {
    (a: A, b: B) => (f(a))(b)
}

def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    //    (a : A) => ((b: B) => c)
    (a: A) => ((b: B) => f(a,b))
}

def sum(a: Int, b: Int) : Int = {
    a + b
}

val curried = curry(sum)
val uncurried = uncurry(curried)
println("uncurried operation: " + uncurried(1,3))

// val firstCurried = curried(1)
// val result = firstCurried(2)
// println("result: " + result)



