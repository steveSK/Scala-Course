import java.lang.Math

def abs(x: Double) : Double =
  if(x < 0) -x else x



def sqrt(x: Double) = {

  def sqrtIter(guess: Double): Double = {
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  }


  def isGoodEnough(guess: Double): Boolean = {
    abs(guess * guess - x) / x < 0.001
  }


  def improve(guess: Double): Double = {
    (guess + x / guess) / 2
  }
  sqrtIter(1.0)
}

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if(a > acc) return 0
    else loop(f
  }
  loop()
}



sqrt(4)

Math.sqrt(0.0001);
sqrt(0.0001)
