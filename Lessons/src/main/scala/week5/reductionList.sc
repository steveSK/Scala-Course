/*abstract class List[T] {
  def reduceLeft(op: (T,T) => T): T = this match {
    case Nil => throw new Error("Nil.reduceLeft")
    case x :: x => (xs foldLeft x)(op)
  }
  def foldLeft[U](z: U)(op: (U,T) => U): U = this match {
    case Nil => z
    case x :: xs => (xs foldLeft op(z,x))(op)
  }
} */

//List(1,2,3)
//List (2,3) foldLeft (0 + 1)
//List(3) foldtLeft (0 + 1) + 2)
//List()  foldLeft (0 + 1) + 2) + 3)

def mapFun[T,U](xs: List[T], f: T => U): List[U] = {
  xs.foldRight(List[U]())((x,y) => f(x) :: y )
}


def lengthFun[T](xs: List[T]): Int =
  xs.foldRight(0)((x,y) => y + 1)

lengthFun(List(1,2,3))
mapFun[Int,Int](List(1,2,3), x => x * x)