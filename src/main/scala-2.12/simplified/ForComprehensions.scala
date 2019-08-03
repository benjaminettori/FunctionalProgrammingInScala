package simplified

import scala.collection.mutable.ArrayBuffer

object ForComprehensions {

  trait List[A]
  case class Cons[A](head: A, tail: List[A]) extends List[A]
  case object Nil extends List[Nothing]

  /***
    * Example to demonstrate how to build custom structures that can
    * be used in a for comprehension
    *
    * Needs the following methods implemented
    *
    * foreach (single for comprehension generator)
    *
    * map  (yield)
    *
    * flatMap (multiple for comprehension generators)
    *
    * withFilter (if statement)
    *
    * @param initialElements
    * @tparam A
    */
  case class Sequence[A](initialElements: A*) {

    // In this example, cheat by using existing collection
    private val backingList : Seq[A] = Seq(initialElements : _*)

    def foreach(block: A => Unit) : Unit = {
      backingList.foreach(block)
    }

    def map[B](f: A => B) : Sequence[B] = {
      Sequence(backingList.map(f) : _*)
    }


    // Note withFilter is used prior to mapping, as a way to reduce the collection of elements that are mapped over
    def withFilter(f : A => Boolean) : Sequence[A] = Sequence(backingList.withFilter(f).map(x => x) : _*)

    def flatMap[B](f: A => Sequence[B]) : Sequence[B] = {
      val seqSeq = backingList.map(f(_))
      val finalSeq : scala.collection.mutable.Buffer[B] = ArrayBuffer()
      for(seq <- seqSeq) {
        for(elm <- seq) {
          finalSeq += elm
        }
      }

      Sequence(finalSeq: _*)
    }
  }

  def main(args: Array[String]) = {
    val test = Sequence(1, 2, 3)
    val testAgain = test.flatMap(i => Sequence(i+1))

    testAgain.foreach(println(_))
  }
}
