package main.scala.common

object ExerciseFive {

  def removeAt[T](i: Int, chars: List[T]): List[T] = {
    if (i >= chars.length) throw new IllegalArgumentException
    else if (i == 0) chars.tail
    else chars.head :: removeAt(i - 1, chars.tail)
  }

  def flattenElem(x: Any): List[Any] = x match {
    case List => flatten(x.asInstanceOf[List[Any]])
    case _ => List(x)
  }

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => xs
    case z :: zs => flattenElem(z) ::: flatten(zs)
  }

  def length[T](xs: List[T]): Int = {
    (xs foldLeft 0) ((a, _) => a + 1)
  }

  def isPrime(n: Int): Boolean = 2 until n exists (n % _ == 0)

  def scalarProduct(xs: List[Int], ys: List[Int]): List[Int] = {
    for ((x, y) <- xs zip ys) yield x * y
  }

  def main(args: Array[String]): Unit = {
    println(removeAt(1, List('a', 'b', 'c', 'd')))
    println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
    println(length(List(1, 2, 3, 4, 5)))
    println(isPrime(3))
    println(isPrime(10))
    println(scalarProduct(List(1,2,3), List(2,4,6)))
  }

}
