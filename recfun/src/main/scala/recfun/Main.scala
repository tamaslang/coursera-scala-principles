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
  def pascal(c: Int, r: Int): Int =
  if (c == 0 || r == 0) 1
  else if (c == 1 || (c + 1) == r) r
  else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def countBalance(chars: List[Char], balance: Int = 0): Boolean = chars match {
      case '(' :: tail => countBalance(tail, balance + 1)
      case ')' :: tail if (balance == 0) => false
      case ')' :: tail => countBalance(tail, balance - 1)
      case e :: tail => countBalance(tail, balance)
      case nil => balance == 0
    }
    countBalance(chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChange(money: Int,  coins: List[Int], count: Int): Int = {
      (money, coins) match {
        case (_, _) if money < 0 => count
        case (0, Nil) => count + 1
        case (_, Nil) => count
        case _ => countChange(money, coins.tail, count) + countChange(money-coins.head, coins, count)
      }
    }
    countChange(money,coins,0)

  }

}
