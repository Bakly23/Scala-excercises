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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || c == r) 1
    else if (c > 0 && r > 0) pascal(c - 1, r - 1) + pascal(c, r - 1)
    else 0
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    var numberOfOpenedParenthisises = 0

    def balanceRec(chars: List[Char]): AnyVal = {
      if (chars.isEmpty) return
      else if (numberOfOpenedParenthisises > -1 && chars.head == '(') numberOfOpenedParenthisises += 1
      else if (numberOfOpenedParenthisises > 0 && chars.head == ')') numberOfOpenedParenthisises -= 1
      else if (numberOfOpenedParenthisises < 1 && chars.head == ')') numberOfOpenedParenthisises = -1
      balanceRec(chars.tail)
    }

    balanceRec(chars)
    numberOfOpenedParenthisises == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else loop(money, coins.tail) + loop(money - coins.head, coins)
    }

    loop(money, coins)
  }
}
