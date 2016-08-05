import java.lang.Math
// example of imports 
import week3.Hello
import week3.{Hello}
import week3._  // instead of Java *

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



sqrt(4)

Math.sqrt(0.0001);
sqrt(0.0001)
