trait List[+T]{
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >:T] (elem: U): List[U] = new Cons(elem,this)
}

class Cons[T](val head: T,val tail: List[T]) extends List[T]{
  def isEmpty = false
}

class Nil extends  List[Nothing]{
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}


def singleton[T](elem: T) = new Cons[T](elem, new Nil)

def nth[T](n: Int, list: List[T]): T =
  if(list.isEmpty || n<0) throw new IndexOutOfBoundsException
  else if(n == 0) return list.head
  else return nth(n-1, list.tail)

object List {
  def apply[T](): List[T] = {
      new Nil
  }

  def apply[T](elem: T): List[T] = {
    new Cons[T](elem,new Nil)
  }
  def apply[T](elem1: T, elem2: T): Unit ={
    return  new Cons[T](elem1,new Cons[T](elem2,new Nil))
  }
}

abstract class IntSet{
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  override def toString = "."
  def union(other: IntSet) = other
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends  IntSet{

  def contains(x: Int): Boolean =
    if(x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem,left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  override def toString = "{" + left + elem + right + "}"

  def union(other: IntSet) = (left.union(right)).union(other).incl(elem)

}
//in scala arrays are not covarient
//val a: Array[NonEmpty] = Array(new NonEmpty(1, new Empty, new Empty))
//val b: Array[IntSet] = a
//b(0) = new Empty
//val s: NonEmpty = a(0)

def f(xs: List[NonEmpty], x: Empty) = xs.prepend(x)
