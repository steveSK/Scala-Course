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
    if (c > r) {
      throw new NoSuchElementException
    }
    if (r == 0 || c == 0 || c == r) {
      1
    }
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanced(chars: List[Char], open: Int): Boolean = {
      if (open < 0 || chars.isEmpty && open > 0) {
        return false
      }
      if (chars.isEmpty && open == 0) {
        return true
      }
      if (chars.head == '(') {
        return balanced(chars.tail, open + 1)
      }
      if (chars.head == ')') {
        return balanced(chars.tail, open - 1)
      }
      balanced(chars.tail, open);

    }
    balanced(chars, 0)

  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChanged(money: Int, coins: List[Int]): Int = {
      if (money == 0)
        return 1
      else if (money > 0 && !coins.isEmpty)
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else
        return 0
    }
    countChanged(money, coins)
  }
}

// 4 = 2 + 2/ 2 + 1 + 1// 1 + 1 +1 + 1