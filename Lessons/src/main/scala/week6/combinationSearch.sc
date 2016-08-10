def isPrime(n : Int): Boolean = (2 until n) forall (x => n % x !=0)


def pairs(n:Int) = {
  ((1 until n) flatMap( i =>
    (1 until i) map (j => (i,j)))) filter (pair => isPrime(pair._1 + pair._2))
}

case class Person(name: String, age: Int)
val persons = List(Person("Stefan",25))
for( p <- persons if p.age > 20) yield p.name


def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map(xy => xy._1*xy._2).sum

def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
  (for ((x,y) <- (xs zip ys)) yield x*y).sum

pairs(5)

