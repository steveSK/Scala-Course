def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]) : List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs,ys)
}

def reverse[T](xs : List[T]): List[T] = xs match {
  case List() => List()
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAt[T](xs: List[T], n: Int): List[T] = {
  if(n<0) throw new NoSuchElementException
  else if(n==0) xs.tail
  else xs.head :: removeAt(xs.tail,n-1)
}

removeAt(List('a','b','c','d'),1)

def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => List()
  case y :: ys => (y match {
    case l:List[Any] => flatten(l)
    case i => List(i)
  }) ::: flatten(ys)

}




flatten(List(List(1,1),2, List(3, List (5,8))))
