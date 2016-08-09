def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge2(msort(fst), msort(snd),ord)
  }
}

def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
  case Nil =>
    ys
  case x :: xs1 =>
    ys match {
      case Nil =>
        ys
      case y :: ys1 =>
        if (x < y) x :: merge(xs1,ys)
        else  y :: merge(xs,ys1)
    }

}


def merge2[T](xs: List[T], ys: List[T],ord : Ordering[T]): List[T] =
  (xs,ys) match {
    case (Nil,ys) => ys
    case (xs,Nil) => xs
    case (x :: xs1, y :: ys1) => if(ord.lt(x,y)) x :: merge2(xs1,ys,ord)  else y :: merge2(xs,ys1,ord)

}

val nums = List(2,-4, 5,6,1)
msort(nums)