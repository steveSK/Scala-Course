def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if(a > b) return acc
    else loop(a + 1, acc + f(a))
  }
  loop(a, 0)
}

def product(f: Int => Int)(a: Int, b: Int):Int = {
  if(a > b) 1 else f(a) * product(f)(a + 1,b)
}

def fact(n :Int) : Int = {
  product(x => x) (1,n)
}

def join(j: (Int,Int) => Int)(f: Int => Int)(a: Int, b: Int) : Int = {
  if(a == b) b
  else   j(f(a),join(j)(f)(a+1,b))
}




join((x,y) => x * y)(x => x)(1,4)



product(x=>x)(1,4)


sum(x => x)(1, 3)

