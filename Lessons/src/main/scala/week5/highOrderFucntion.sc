def map[T,U](list: List[T], f: T => U): List[U] = list match {
  case Nil => List()
  case x :: xs => f(x) :: map(xs,f)
}

def squareList(xs: List[Int]): List[Int] = {
  xs match {
    case Nil => Nil
    case y :: ys => y*y :: squareList(ys)
  }
}

def squareList2(xs: List[Int]): List[Int] = {
  xs.map(x => x*x)
}

val l = List(2,3,4)
val l2 = squareList(l)

def pack[T](xs: List[T]): List[List[T]] = xs match  {
  case Nil => Nil
  case x :: xs1 => val part = xs.partition(y => y == x); part._1 :: pack(part._2)
}

pack(List('a','a','a','b','b','c', 'c','a'))


def encode2[T](xs : List[T]) : List[(T,Int)] =  {
    def encode0[T](xs0: List[List[T]]): List[(T,Int)] = xs0 match {
      case List() => List()
      case y :: ys => (y.head, y.length) :: encode0(ys)
    }
    encode0(pack(xs))
  }

def encode[T](xs : List[T]): List[(T,Int)] = {
  pack(xs) map (ys => (ys.head, ys.length))
}
encode(List('a','a','a','b','b','c', 'c','a'))

