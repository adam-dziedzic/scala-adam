package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  //  def pascal(c: Int, r: Int): Int = {
  //    @annotation.tailrec
  //    def loop(c: Int, r: Int, l: List[Int]): Int = {
  //      if (c == 0 && r == 0) 1
  //      else if (c == 0 || c == r) 1
  //      else 
  //    }
  //    loop(c, r, List(1))
  //  }
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @annotation.tailrec
    def loop(chars: List[Char], parenthesis: List[Char]): Boolean = {
      println("Current char: " + chars.head)
      if (chars.isEmpty) {
        if (parenthesis.isEmpty) true else false
      } else if (chars.head == '(') loop(chars.tail, '(' :: parenthesis)
      else if (chars.head == ')' && parenthesis.head == '(') loop(chars.tail, parenthesis.tail)
      else if (chars.head == ')') false
      else loop(chars.tail, parenthesis)
    }
    println(loop(chars,List()))
    loop(chars, List())
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
