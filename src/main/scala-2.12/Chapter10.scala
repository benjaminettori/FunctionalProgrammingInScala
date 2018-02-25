/**
  * Created by bettori on 1/20/2018.
  */

import Chapter6.ScalaState
import Chapter8._
import Chapter7._
import java.util.concurrent.Callable;

object Chapter10 {

  trait Monoid[A] {
    def op(x: A, y: A): A
    def identity: A
  }

  // Using Monoid as argument to fold operation
  // in order to concatenate list arguments together
  // can use foldLeft or foldRight indiscriminately because Monoid op is associative
  def concatenate[A](as: List[A], m: Monoid[A]) : A = as.foldLeft(m.identity)(m.op)

  // Ex 10.5
  // We can transform a type (A) that does not have a monoid into a type that does (B)
  // before concatenating
  // If type has a monoid, concatenation is straightforward
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B) : B = concatenate(as.map(f), m)

  // Ex 10.6
  // express foldRight as function of foldMap
  def foldRightMonoid[A, B](as: List[A], z: B)(f : (A, B) => B): B = {
    val AtoB: A => (B => B) = a => f(a, _:B)

    val monoid = new Monoid[B => B] {
      def op(x: B => B, y: B => B) : B => B = b => y(x(b))
      def identity : B => B = b => b
    }

    foldMap(as, monoid)(AtoB)(z)
  }

  // for fold left implementation
  // we use a monoid dual (flip the order of the arguments)
  def foldLeftMonoid[A, B](as: List[A])(z: B)(f: (B, A) => B) : B = {
    val AtoB : A => (B => B) = a => f(_, a)

    val monoid = new Monoid[B => B] {
      def op(x: B => B, y: B => B) : B => B = b => x(y(b))
      def identity : B => B = b => b
    }

    foldMap(as, monoid)(AtoB)(z)
  }

  val stringMonoid = new Monoid[String] {
    def op(a: String, b: String) = a + b
    def identity = ""
  }

  // Ex 10.7
  // Split sequence in half before folding.
  // Do it recursively.
  // This is a more efficient implementation
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B) : B = {
    val seqLength = v.length

    if(seqLength == 0) {
      m.identity
    } else if (seqLength == 1) {
      f(v(0))
    } else {
      val splitIndex = seqLength / 2
      val (l, r) = v.splitAt(splitIndex)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }
  // Ex 10.8
  // Create par to map monoid A to monoid Par[A
  // Can use map2
  // Or explicit definition
  def par[A](m: Monoid[A]) : Monoid[Par[A]] = {
    new Monoid[Par[A]] {
      //def test(a: Par[A], b: Par[A]) : Par[A] = Par.map2(a, b)(m.op)
      def op(a: Par[A], b: Par[A]): Par[A] = es => {
        es.submit(new Callable[A] {
          def call() : A = m.op(Par.run(es)(a).get(), Par.run(es)(b).get())
        })
      }

      override def identity: Par[A] = Par.unit(m.identity)
    }
  }

  /***
    * Ex 10.8
    *
    */
  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B) : Par[B] = {
    //val test = Chapter7.flatMap(Par.parMap(v.toList)(f))(bs => foldMapV(bs.toIndexedSeq, par(m))(b => Par.unit(b)))
    val parF : A => Par[B] = a => Par.lazyUnit(f(a))
    foldMapV(v, par(m))(parF)
  }

  /***
    * Ex 10.9
    *
    */

  def isOrdered(v: IndexedSeq[Int]) : Boolean = {
    v.foldLeft((true, v.head))((c, n) => {
      val (isOrd, prv) = c
      (isOrd && (n >= prv), n)
    })._1
  }

  /***
    *
    * This monoid will not work for parallel operations
    * The reason is that a list can be split into multiple ordered segments
    * even if the list itself is not ordered
    * This monoid does not handle this corner case
    *
    * Instead, the full solution tracks the range of values in each segment of the list
    * and makes sure they don't overlap
    */
  def orderedMonoid = new Monoid[(Boolean, Int)] {
    def op(a: (Boolean, Int), b : (Boolean, Int)) : (Boolean, Int) = ( a._1 && b._1 && a._2 <= b._2, b._2)

    override def identity = (true, Int.MinValue)
  }

  def testOrder(v: IndexedSeq[Int]) : Boolean = {
    foldMap(v.toList, orderedMonoid)(s => (true, s))._1
  }

  /***
    * Ex 10.10
    *
    */

  trait WC
  case class Stub(char: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def containsAny(testString: String, subStrings: Array[String]) : Boolean = {
    subStrings.map(s => testString.contains(s)).reduce((x, y) => x || y)
  }

  val wcMonoid = new Monoid[WC] {
    def op(a: WC, b: WC) : WC = {
      (a, b) match {
        case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
        case (Stub(c), Part(ls, n, rs)) => Part(c+ls, n, rs)
        case (Part(ls, n, rs), Stub(c)) => Part(ls, n, rs+c)
        case (Part(ls1, n1, rs1), Part(ls2, n2, rs2)) => Part(ls1, n1 + n2 + (if((rs1 + ls2).isEmpty) 0 else 1) , rs2)
      }
    }
    def identity : WC = Stub("")
  }

  /***
    * Ex 10.11
    *
    */

  def stringToWcList(s: String) : List[WC] = {
    val stringLength = s.length

    if(stringLength > 10) {
      val (s1, s2) = s.splitAt(stringLength / 2)
      stringToWcList(s1) ::: stringToWcList(s2)
    } else {
      List(stringToWc(s))
    }
  }

  def countWordsInString(s: String) : Int = {
    stringToWcList(s).foldLeft(wcMonoid.identity)(wcMonoid.op) match {
      case Stub(_) => 0
      case Part(l, n, r) => unstub(l) +  n + unstub(r)
    }
  }

  def stringToList(s: String): List[String] = {
    val stringLength = s.length

    if(stringLength > 10) {
      val (s1, s2) = s.splitAt(stringLength / 2)
      stringToList(s1) ::: stringToList(s2)
    } else {
      List(s)
    }
  }

  def stringToWc(s: String) : WC = {
    if(!s.contains(" ")) Stub(s)
    else {
      val numberSpaces = s.map(c => c == ' ').count(a => a)
      val start = s.substring(0, s.indexOf(' '))
      val end = s.substring(s.lastIndexOf(' '), s.length - 1)
      Part(start, numberSpaces - 1, end)
    }
  }

  def unstub(s : String) : Int = s.length min 1

  def countWordsInString_v2(s: String) : Int = {
    foldMap(stringToList(s), wcMonoid)(stringToWc) match {
      case Stub(_) => 0
      case Part(l, n, r) => unstub(l) + n + unstub(r)
    }
  }

  /***
    *
    * Foldable data structures
    */

  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B) : B
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B) : B
    def foldMap[A, B](as: F[A], m: Monoid[B])(f: A => B) : B
    def concatenate[A](as: F[A], m: Monoid[A]) : A = foldLeft(as)(m.identity)(m.op)
    }

  /*** Ex 10.12 */
  object ListFoldable extends Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) : B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A], m: Monoid[B])(f: (A) => B): B = ???
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

    val v = IndexedSeq(1,2,3)
    val vf = IndexedSeq(2, 1, 3)
    println(isOrdered(v))
   // println(testOrder(v))
    println(testOrder(vf))

    val testString = " Lorem ipsum dolor Emma tempus fugit Judi birthday"
    println(countWordsInString(testString))
    println(countWordsInString_v2(testString))
  }
}

