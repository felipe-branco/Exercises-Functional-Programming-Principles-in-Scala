package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    print(balance("(if (zero? x) max (/ 1 x))".toList))
    println()
    print(balance("())(".toList))
    println()
    print(balance(":-)".toList))
    println()
    print(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println()
    print(countChange(4, List(1, 2)))
    println()
    print(sum(x => x*x)(3,5))
    println()
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def isBoundarie(c: Int, r: Int): Boolean = ( (c == 0) || (r == 0) || (r == c) )
    if (isBoundarie(c,r)) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def checkBalance(chars: List[Char], open_or_close: Int): Boolean = {
      if (open_or_close < 0) false
      else if (chars.isEmpty) open_or_close == 0
      else if (chars.head == '(') checkBalance(chars.tail, open_or_close + 1)
      else if (chars.head == ')') checkBalance(chars.tail, open_or_close - 1)
      else checkBalance(chars.tail, open_or_close)
    }
    checkBalance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def _countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else _countChange(money, coins.tail) + _countChange(money - coins.head, coins)
    }
    _countChange(money, coins)
  }

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a+1, f(a)+acc)
    }
    loop(a, 0)
  }
}
