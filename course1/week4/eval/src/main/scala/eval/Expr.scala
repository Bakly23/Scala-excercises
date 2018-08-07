package eval

import eval.Example.{Number, Prod, Sum, Var}

trait Expr {
  def print(): String = this match {
    case Number(x) => x.toString
    case Sum(x, y) => x.print() + " + " + y.print()
    case Var(x) => x
    case _ => print()
  }
}
