val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)

-x
x < y
x - y - z
y + y


class Rational(x: Int, y: Int){
   require(y != 0, "denominator must be nonzero") // condition which have to be fullfiled when the instance is created
   private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b,a % b)
   private val g = gcd(x,y)
   def nom = x / g
   def denom = y / g

   def this(x: Int) = this(x,1)

   def <(that : Rational) = nom*that.denom < that.nom*denom

   def max(that: Rational) = if(this < that) that else this

   def +(that: Rational) = {
     new Rational(nom*that.denom + that.nom*denom,denom*that.denom)
   }

  def unary_- = {
     new Rational(-nom,denom)
  }

  def -(that: Rational) = {
    this + -that
  }

   override def toString = nom + "/" + denom
}
