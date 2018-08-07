package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck._

import scala.util.Random

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      p <- arbitrary
      h <- oneOf(const(empty), genHeap)
    } yield insert(p, h)
  )
  val rand = new Random()

  def arbitrary: Gen[A] = Arbitrary.arbInt.arbitrary

  def const(heap: H): Gen[H] = Gen.const(heap)

  def oneOf(heap1: Gen[H], heap2: Gen[H]): Gen[H] = Gen.oneOf(heap1, heap2)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("minimum2") = forAll { (h: H) =>
    val a1: Int = arbitrary.sample.get
    val a2: Int = arbitrary.sample.get
    val m = Math.min(a1, a2)
    val foundMin = findMin(insert(a2, insert(a1, empty)))
    foundMin == m
  }

  property("empty") = forAll { (h: H) =>
    isEmpty(deleteMin(insert(arbitrary.sample.get, empty)))
  }

  property("min-sorted") = forAll { (h: H) =>
    def findOutSortness(h: H): Boolean = {
      if (isEmpty(h) || isEmpty(deleteMin(h))) true
      else if (findMin(h) > findMin(deleteMin(h))) false
      else findOutSortness(deleteMin(h))
    }

    findOutSortness(h)
  }

  property("min-of-meld") = forAll { (h1: H, h2: H) =>
    def minOfProbableEmpty(h: H) = if (isEmpty(h)) Int.MaxValue else findMin(h)

    val m = Math.min(minOfProbableEmpty(h1), minOfProbableEmpty(h2))
    minOfProbableEmpty(meld(h1, h2)) == m
  }

  property("meld") = forAll { (a1: A, a2: A) =>
    val h = insert(a2, insert(a1, empty))
    val min = findMin(h)
    val max = findMin(deleteMin(h))
    if (a1 < a2) min == a1 && max == a2 else min == a2 && max == a1
  }

  property("delete-min") = forAll { (h: H) =>
    val newH = insert(3, insert(2, insert(1, empty)))
    findMin(deleteMin(newH)) == 2
  }

}
