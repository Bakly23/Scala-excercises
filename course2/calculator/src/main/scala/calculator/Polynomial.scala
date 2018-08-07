package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(Math.pow(b(), 2) - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var(Set((-b() + Math.sqrt(delta())) / (2 * a()), (-b() - Math.sqrt(delta())) / (2 * a())).filter(!_.isNaN))
  }
}
