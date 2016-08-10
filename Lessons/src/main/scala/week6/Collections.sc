val xs = Array(2,5,7,87)
xs map (x => x*2)

val s = "Hello World"
s filter (x => x.isUpper)

val range: Range = 1 until 5
val range2: Range = 1 to 5
1 to 10 by 3

s exists (c => c.isUpper)
s forall (c => c.isUpper)

val pairs = List(1,2,3) zip s
pairs.unzip

s flatMap(c => List('.',c))

xs.sum
xs.max


def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map(xy => xy._1*xy._2).sum



def isPrime(n : Int): Boolean = (2 until n) forall (x => n % x !=0)

isPrime(7)
isPrime(9)
isPrime(4)
isPrime(19)
isPrime(1)


