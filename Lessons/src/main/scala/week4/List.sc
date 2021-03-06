def isort(xs: List[Int]): List[Int] = xs match  {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match  {
  case List() => x :: Nil
  case y :: ys  => if(x < y) x :: xs else y :: insert(x,ys)

}


val list = List(7,9,2,5)
isort(list)