package idealized.scala

abstract  class Boolean{
  def ifThenElse[T](t: => T, e: => T): T


  def && (x: => Boolean): Boolean = ifThenElse(x,false)
  def || (x: => Boolean): Boolean = ifThenElse(true,x)
  def unary_!(): Boolean          = ifThenElse(false, true)

  def == (x: Boolean): Boolean    = ifThenElse(x,x.unary_!)
  def != (x: Boolean): Boolean    = ifThenElse(x.unary_!,x)

  def <  (x: Boolean): Boolean    = ifThenElse(false,x)
  def >  (x: Boolean): Boolean    = ifThenElse(true,x)


}

object true extends Boolean {
  def ifThenELse[T](t: => T, e: => T) = t
}

object false extends Boolean {
  def ifThenELse[T](t: => T, e: => T) = e
}

//bounds
  // upperboud
S <: IntSet  // means S is subtype of IntSet
// lower bound
S >: IntSet // means S is supertype of IntSet
