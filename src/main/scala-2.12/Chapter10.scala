/**
  * Created by bettori on 1/20/2018.
  */

import Chapter6.ScalaState
import Chapter8._
object Chapter10 {

  trait Monoid[A] {
    def op(x: A, y: A): A
    def identity: A
  }

  val stringMonoid = new Monoid[String] {
    def op(a: String, b: String) = a + b
    def identity = ""
  }

  def main(args: Array[String]): Unit = {
    /*** Ex 10.1
      *
      * Implement simple monoids
      */
    val intAddition = new Monoid[Int] {
      def op(a: Int, b: Int) = a + b
      def identity = 0
    }

    val intMultiplication = new Monoid[Int] {
      def op(a: Int, b: Int) = a * b
      override def identity = 1
    }

    val booleanOr = new Monoid[Boolean] {
      def op(a: Boolean, b: Boolean) = a || b

      override def identity = false
    }

    val booleanAnd = new Monoid[Boolean] {
      override def op(x: Boolean, y: Boolean): Boolean = x && y

      override def identity: Boolean = true
    }

    /***
      * Ex 10.2
      * Combine option values
       */

    def optionMonoid[T] = new Monoid[Option[T]] {
      override def op(x: Option[T], y: Option[T]): Option[T] = {
       x orElse y
      }

      override def identity: Option[T] = None
    }

    /***
      * Ex 10.3
      * monoid for endofunctions
      */
    def endoMonoid[A] = new Monoid[A => A] {
      override def op(x: A => A, y: A => A): (A => A) = a => y(x(a))
      override def identity: A => A = a => a
    }

    /***
      * Ex 10.4
      * property for monoid laws
      */
    def chooseTuple[A](gen: Gen[A]): Gen[(A, A)] = Gen(ScalaState(rng => {
      val a = gen.sample.run(rng)
      val b = gen.sample.run(a._2)
      ((a._1, b._1), b._2)
    }))

    def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = Prop.forAll(chooseTuple(gen))(p => m.op(p._1, m.op(p._2, m.identity)) == m.op(p._1, p._2) && m.op(m.op(p._1, p._2), m.identity) == m.op(p._1, p._2))

    /***
      * More complete test for 10.4
      * Can combine props using &&
      * Can use for comprehension to get multiple values from the generator
      */
    def test[A](m: Monoid[A], gen: Gen[A]): Prop = Prop.forAll(
      for{x <- gen; y <- gen; z <- gen} yield (x, y, z)
    )(t => m.op(t._1, m.op(t._2, t._3)) == m.op(m.op(t._1, t._2), t._3)) && Prop.forAll(gen)(a => m.op(m.identity, a) == a && m.op(a, m.identity) == a)
  }
}
