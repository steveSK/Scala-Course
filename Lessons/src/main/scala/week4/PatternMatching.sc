
trait Expr
case class Number(n: Int) extends Expr
case class Var(v: String) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr



def eval (e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
}

def show(e: Expr): String = e match {
  case Number(n) => String.valueOf(n)
  case Sum(e1, e2) => show(e1) + " + " + show(e2)
  case Var(s) => s
  case Prod(e1,e2) =>  addParr(e1) + " * " + addParr(e2)
}

def addParr(e: Expr): String = e match {
  case Sum(e1, e2) =>  "(" + show(e) + ")"
  case _ => show(e)
}

show(Sum(Prod(Number(2),Var("x")),Var("y")))
show(Prod(Sum(Number(2),Var("x")),Var("y")))