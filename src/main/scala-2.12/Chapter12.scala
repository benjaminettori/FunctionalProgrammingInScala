import java.util.Date

import Chapter10.{Foldable, Monoid}
import Chapter11.{Mon, Monad}
import Chapter6.{ScalaState, State}

object Chapter12 {

  trait Functor[F[_]] {
    def map[A, B](t: F[A])(f: A => B): F[B]
  }

  /***
    *
    * Applicative trait uses map2 and unit as it's base methods (or apply and unit)
    */
  trait Applicative[F[_]] extends Functor[F] {
    def map2[A, B, C](a: F[A], b: F[B])(f: (A, B) => C) : F[C]
    def unit[A](a: => A) : F[A]

    def map[A, B](fa: F[A])(f: A => B) : F[B] = map2(fa, unit(()))((a, _) => f(a))
    def traverse[A, B](as: List[A])(f: A => F[B]) : F[List[B]] = as.foldRight( unit(List[B]()) ) ((a, flb) => map2(f(a), flb) ((b, lb) => b :: lb) )

    // Ex 12.1
    def sequence[A](fas: List[F[A]]) : F[List[A]] = traverse(fas)(fa => fa)
    def replicate[A](n: Int, fa: F[A]) : F[List[A]] = sequence(List.fill(n)(fa))
    def product[A, B](fa: F[A], fb: F[B]) : F[(A, B)] = map2(fa, fb) ((a, b) => (a, b))

    // Ex 12.2
    def apply[A, B](fab: F[A => B])(fa: F[A]) : F[B] = map2(fab, fa) ((fab, a) => fab(a))
    def map2FromApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C) : F[C] = apply(apply[A, B => C](unit({a: A => f(a, _)}))(fa))(fb)

    // Ex 12.3
    def map3[A, B, C, D](fa: F[A], fb : F[B], fc: F[C])(f: (A, B, C) => D) : F[D] = apply(apply(apply[A, B => C => D](unit({a: A => {b: B => f(a, b, _)}}))(fa))(fb))(fc)
    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E) : F[E] = apply(apply(apply(apply[A, B => C => D => E](unit({a: A => {b: B => {c : C => f(a, b, c, _)}}}))(fa))(fb))(fc))(fd)

    // Ex 12.8

    def product[G[_]](G: Applicative[G]) : Applicative[({type f[x] = (F[x], G[x])})#f] = {
      val self = this
      new Applicative[({type f[x] = (F[x], G[x])})#f] {
        override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

        override def map2[A, B, C](a: (F[A], G[A]), b: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = (self.map2(a._1, b._1)(f), G.map2(a._2, b._2)(f))
      }
    }

    // Ex 12.9
    def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
      val self = this
      new Applicative[({type f[x] = (F[G[x]])})#f] {
        override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

        override def map2[A, B, C](a: F[G[A]], b: F[G[B]])(f: (A, B) => C): F[G[C]] = self.map2(a, b)((ga, gb) => G.map2(ga, gb)(f))
      }
    }

    // Ex 12.12
    def sequenceMap[K, V](map: Map[K, F[V]]) : F[Map[K, V]] = {
      map.foldRight(unit(Map[K, V]()))((kfv, fm) => map2(kfv._2, fm)((el, m) => m + (kfv._1 -> el)))
    }
  }


  type Const[M, B] = M

  implicit def monoidApplicative[M](m : Monoid[M]) = new Applicative[({type f[x] = Const[M, x]})#f] {
    override def unit[A](a: => A): M = m.identity

    override def map2[A, B, C](a: M, b: M)(f: (A, B) => C): M = m.op(a, b)
  }

  /***
    * Traverse extends foldable
    * From monoids to applicative functors
    */


  object Monad {

    def stateMonad[S] = new Mon[({type f[x] = ScalaState[S, x]})#f] {
      def unit[A](a: => A): ScalaState[S, A] = ScalaState(s => (a, s))

      override def flatMap[A, B](st: ScalaState[S, A])(f: A => ScalaState[S, B]): ScalaState[S, B] =
        st flatMap f
    }
  }

  trait Traverse[F[_]] extends Functor[F] with Foldable[F]{ self =>
    def traverse[G[_]: Applicative, A, B](f: F[A])(g: A => G[B]) : G[F[B]]
    def sequence[G[_], A](f: F[G[A]])(implicit G: Applicative[G]): G[F[A]] = traverse(f)(f => f)

    // Ex 12.14
    def map[A, B](fa: F[A])(f: A => B) : F[B] = {
      type Id[A] = A
      val idMonad = new Applicative[Id] {
        def unit[A](a: => A) = a

        override def map2[A, B, C](a: Id[A], b: Id[B])(f: (A, B) => C): Id[C] = ???
      }

      val test = traverse[Id, A, B](fa)(a =>f(a))(idMonad)
      test
    }

    // Foldable section
    override def foldMap[A, B](as: F[A], m: Monoid[B])(f: A => B): B = traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(m))

    override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = ???

    override def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = ???

    // Traverse state
    def traverseS[S, A, B](fa: F[A])(f: A => ScalaState[S, B]) : ScalaState[S, F[B]] = ??? //traverse[({type f[x] = ScalaState[S, x]})#f, A, B](fa)(f)(Monad.stateMonad[S])

    import ScalaState._
    def zipWithIndex[A](ta : F[A]) : F[(A, Int)] = traverseS(ta)((a : A) => for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i)).run(0)._1

    def toListFunc[A](fa: F[A]) : List[A] = traverseS(fa)((a: A) => for {
      as <- get[List[A]]
      _ <- set(a :: as)
    } yield ()).run(Nil)._2.reverse

    def mapAccum[S,A,B](fa: F[A], s :S)(f: (A, S) => (B, S)) : (F[B], S) = traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

    // Ex 12.16

    def reverse[A](fa: F[A]) : F[A] = mapAccum(fa,toListFunc(fa).reverse)((_, s) => (s.head, s.tail))._1


    // Ex 12.17
    def foldLeftDefault[A, B](as: F[A])(z: B)(f: (B, A) => B) : B = mapAccum(as, z)((a, b) => (b, f(b, a)))._2

    def zipL[A,B](fa:F[A], fb: F[B]) : F[(A, Option[B])] = {
      (mapAccum(fa, toList(fb))  {
        case (a, Nil) =>((a, None), Nil)
        case (a, b :: bs) => ((a, Some(b)), bs)
      })._1

      // Same thing as below

//      mapAccum(fa, toList(fb))((a, lb) => {
//        (a, lb) match {
//          case (a, Nil) => ((a, None), Nil)
//          case (a, b :: bs) => ((a, Some(b)), bs)
//        }
//      })._1
    }

    def zipR[A, B](fa : F[A], fb : F[B]) : F[(Option[A], B)] = {
      (mapAccum(fb, toList(fa)) {
        case (b, Nil) => ((None, b), Nil)
        case (b, a :: as) => ((Some(a), b), as)
      })._1
    }

    /***
      *
      Ex 12.18
      */

    def fuse[G[_], H[_], A, B](fa : F[A])(f: A => G[B], g : A => H[B])
                              (G: Applicative[G], H : Applicative[H]) : (G[F[B]], H[F[B]]) = {
     traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G.product(H))
    }

    /***
      * Ex 12.19
      */
    def compose[G[_]](implicit G: Traverse[G]) : Traverse[({type f[x] = F[G[x]]})#f] = {
      val test = new Traverse[({type f[x] = F[G[x]]})#f] {
        override def traverse[H[_] : Applicative, A, B](f: F[G[A]])(g: A => H[B]): H[F[G[B]]] = self.traverse(f)(x => G.traverse(x)(g))
      }
      test
    }

    /***
      * Ex 12.20
      */
    def composeM[G[_], H[_]](G: Mon[G], H: Mon[H], T: Traverse[H]) : Mon[({type f[x] = G[H[x]]})#f] = {
      new Mon[({type f[x] = G[H[x]]})#f] {



        override def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))

        override def flatMap[A, B](gha: G[H[A]])(f: A => G[H[B]]): G[H[B]] = ??? //G.flatMap(gha)(na => G.map(T.traverse(na)(f))(H.join))
//          val ghhb = T.traverse(na)(f)
//          val ghb = G.map(ghhb)(H.join)
      }
    }
  }



  case class Iteration[A](a: A, f: A => A, n: Int) {
    def foldMap[B](g: A => B)(M: Monoid[B]): B = {
      def iterate(n: Int, b: B, c: A): B =
        if (n <= 0) b else iterate(n-1, g(c), f(a))
      iterate(n, M.identity, a)
    }

    def map[B](g: A => B) : Iteration[B] = {
      def test(a : A) : B = g(f(a))
      Iteration(g(f(a)), (b: B ) => b, n)
    }
  }

  val itFunc = new Functor[Iteration] {
    override def map[A, B](t: Iteration[A])(f: A => B): Iteration[B] = t.map(f)
  }

  def plusOne(a: Int) : Int = a + 1
  val myit = Iteration(1, (a: Int) => a + 1, 6)
  val test = myit.map((n: Int) => n.toString)

  case class Tree[+A](head: A, tail: List[Tree[A]])
  // Ex 12.13
  class OptionTraversable extends Traverse[Option] {
    override def traverse[G[_], A, B](f: Option[A])(g: A => G[B])(implicit M: Applicative[G]): G[Option[B]] = {
      f match {
        case Some(fa) => M.map(g(fa))(Some(_))
        case None => M.unit(None)
      }
    }

    override def map[A, B](t: Option[A])(f: A => B): Option[B] = ???
  }

  class ListTraversable extends Traverse[List] {
    override def traverse[G[_], A, B](f: List[A])(g: A => G[B])(implicit M: Applicative[G]): G[List[B]] = {
      f.foldRight(M.unit(List[B]()))((a, b) => M.map2(g(a), b)(_ :: _))
    }

    override def map[A, B](t: List[A])(f: A => B): List[B] = ???
  }

  class TreeTraversable extends Traverse[Tree] {
    override def traverse[G[_], A, B](f: Tree[A])(g: A => G[B])(implicit M: Applicative[G]): G[Tree[B]] = {
      val listTraverse = new ListTraversable
      M.map2(g(f.head), listTraverse.traverse(f.tail)(ta => traverse(ta)(g)) : G[List[Tree[B]]])((b, ltb) => Tree(b, ltb))
    }


    override def map[A, B](t: Tree[A])(f: A => B): Tree[B] = ???
  }

  val streamApplicative = new Applicative[Stream] {
    def unit[A](a : => A) : Stream[A] = Stream.continually(a)
    def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C) : Stream[C] = (a zip b).map(s => f.tupled(s))

    // sequence for stream returns a Stream[List[A]] which is a stream of lists where each entry in the list is the next
    // value of each stream that is passed to sequence.
  }

  // Ex 12.5
  def eitherMonad[E]: Mon[({type f[x] = Either[E, x]})#f] = new Mon[({type f[x] = Either[E, x]})#f] {
    def unit[A](a: => A) : Either[E, A] = Right(a)

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.flatMap(a => f(a))
  }

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]


  // Ex 12.6
  def valApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)
    override def map2[A, B, C](a:Validation[E, A], b: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
      (a, b) match {
        case (Success(va), Success(vb)) => Success(f(va, vb))
        case (Success(_), Failure(h, t)) => Failure(h, t)
        case (Failure(h, t), Success(_)) => Failure(h, t)
        case (Failure(ha, ta), Failure(hb, tb)) => Failure(ha, ta ++ (hb +: tb))
      }
    }
  }

  case class WebForm(name: String, birthDate: Date, phoneNumber: String)

  def validName(name: String) : Validation[String, String] = if(name != "") Success(name) else Failure("Must enter non empty string for name")
  def validDate(birthDate: String) : Validation[String, Date] =
    try {
      import java.text._
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthDate))
    } catch {
      case _ => Failure("Birth date must have year, month and day")
    }

  def validPhoneNumber(phoneNumber: String) : Validation[String, String] = if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber) else Failure("Incorrect phone number format")

  /***
    * This method will accumulate all errors from the validation method into a Failure object
    * This is the advantage of using an Applicative vs. a Monad, because the Monad would halt execution
    * after the first error.5
    * @param name
    * @param birthDate
    * @param phoneNumber
    * @return
    */
  def validWebForm(name: String, birthDate: String, phoneNumber: String) : Validation[String, WebForm] = {
    valApplicative.map3(validName(name), validDate(birthDate), validPhoneNumber(phoneNumber))(new WebForm(_, _, _))
  }




  def main(args: Array[String]) : Unit = {}

}
