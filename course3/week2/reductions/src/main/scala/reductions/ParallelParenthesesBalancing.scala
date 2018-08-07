package reductions

import common.parallel
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)
  @volatile var seqResult = false
  @volatile var parResult = false

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 1000000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    chars.foldLeft(0)((accum, c) => {
      if (accum < 0) return false
      c match {
        case '(' => accum + 1
        case ')' => accum - 1
        case _ => accum
      }
    }) == 0
  }

  /** Returns `true` iff the parentheses in the inpu( `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      var i = idx
      var firstClosedBrackets = 0
      var totalAccum = 0
      var wasThereOpenBracket = false
      while(i < until) {
        val c = chars(i)
        if(c == ')') {
          totalAccum = totalAccum - 1
          if(!wasThereOpenBracket) {
            firstClosedBrackets = firstClosedBrackets + 1
          }
        } else if(c == '(') {
          wasThereOpenBracket = true
          totalAccum = totalAccum + 1
        }
        i = i + 1
      }
      (firstClosedBrackets, totalAccum)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = (until + from) / 2
        val (l, r) = parallel(reduce(from, mid), reduce(mid, until))
        (l._1, if (l._2 >= r._1) l._2 + r._2 else Integer.MIN_VALUE)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
