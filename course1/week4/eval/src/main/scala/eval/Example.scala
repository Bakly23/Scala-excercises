package eval

object Example {
  case class Number(x: Int) extends Expr
  case class Sum(x: Expr, y: Expr) extends Expr
  case class Prod(x: Expr, y: Expr) extends Expr {
    private def printProd(expr: Expr) = expr match {
      case Sum(sumX, sumY) => "(" + expr.print() + ")"
      case _ => expr.print()
    }

    override def print(): String = {
      printProd(x) + " * " + printProd(y)
    }
  }
  case class Var(x: String) extends Expr

  def main(args: Array[String]): Unit = {
    val sum: Expr = Sum(Prod(Number(2), Var("x")), Var("y"))
    println(sum.print())
    val prod: Expr = Prod(Sum(Number(2), Var("x")), Var("y"))
    println(prod.print())

  }
}
