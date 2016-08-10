package week6

/**
  * Created by stefan on 8/10/16.
  */
object Test {
  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if (isSafe)(col, queens)
        } yield col :: queens
    }
    placeQueens(n)
  }

  def isSafe(col: Int, q: List[Int]): Boolean = {
    val row = q.length
    val queensWithRow = (row -1 to 0 by -1) zip q
    queensWithRow forall {
      case (r,c) => col != c && Math.abs(col -c) != row -r
    }

  }
  def main(args: Array[String]) {
    println(queens(4))
  }

}
