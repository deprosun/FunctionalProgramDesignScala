package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  def remainingMin(ts: H, as: List[Int]): List[Int] = {
    if (isEmpty(ts)) as
    else findMin(ts) :: remainingMin(deleteMin(ts), as)
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    v <- oneOf(const(empty), genHeap)
  } yield insert(k, v)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("correctMin") = forAll { (x: Int, y: Int) =>
    val h = insert(y, insert(x, empty))
    val smallest = if (x < y) x else y
    findMin(h) == smallest
  }

  property("correctDelete") = forAll { (x: Int) =>
    val h = insert(x, empty)
    deleteMin(h) == empty
  }

  property("correctOrderingUsingFind") = forAll { (h1: H, h2: H) =>
    val melded1 = meld(h1, h2)
    val min1 = findMin(h1)
    val melded2 = meld(deleteMin(h1), insert(min1, h2))
    remainingMin(melded1, Nil) == remainingMin(melded2, Nil)
  }

}
