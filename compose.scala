def compose[A,B,C](f: B=>C, g: A=>B): A => C = {
    (a: A) => f(g(a))
}

def multiply2(a: Int) : Int = {
    a*2
}

def add5(a: Int) : Int = {
    a+5
}

val comfun = compose(multiply2, add5)
// expected result: 18
println("result: " + comfun(4))