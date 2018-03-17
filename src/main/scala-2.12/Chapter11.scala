/**
  * Created by bettori on 3/11/2018.
  */
object Chapter11 {

  /***
    * F is the generic type.
    * A functor is a trait that defines a map on a generic type
    * @tparam F : The generic type the functor applies to.
    */
  trait Functor[F[_]] {
    def map[A, B](o: F[A])(f: A => B): F[B]

    // Other useful abstractions we get from the map implementation
    def distribute[A, B](fab: F[(A, B)]) : (F[A], F[B]) = {
      (map(fab)(ab => ab._1), map(fab)(ab => ab._2))
    }

    def codistribute[A, B](eab: Either[F[A], F[B]]) : F[Either[A, B]] = {
      eab match {
        case Left(a) => map(a)(a => Left(a))
        case Right(b) => map(b)(b => Right(b))
      }
    }
  }

  // Example of implementing the functor trait
  val listFunctor = new Functor[List] {
    def map[A, B](l : List[A])(f: A => B) : List[B] = l.map(a => f(a))
  }

  /***
    * Monad
    * Extends functor trait since it implements map
    * Base methods are flatMap and unit
    * Can implement map2 and map from the base methods.
    */

  trait Mon[F[_]] extends Functor[F] {
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C) : F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))

    def map[A, B](fa : F[A])(f: A => B) : F[B] = flatMap(fa)(a => unit(f(a)))

    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def unit[A](a: => A): F[A]
  }

  // List is a Monad

  val listMonad = new Mon[List] {
    def unit[A](a: => A) : List[A] = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]) : List[B] = fa.flatMap(a => f(a))
  }


}
