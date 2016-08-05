import java.util.NoSuchElementException



trait List[T]{
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T,val tail: List[T]) extends List[T]{
  def isEmpty = false
}

class Nil[T] extends  List[T]{
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}


def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])

def nth[T](n: Int, list: List[T]): T =
    if(list.isEmpty || n<0) throw new IndexOutOfBoundsException
    else if(n == 0) return list.head
    else return nth(n-1, list.tail)


val list = new Cons(3,new Cons(2, new Cons(1,new Nil)))

val variable = nth(0,list)