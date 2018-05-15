object Chapter12 {

  trait Functor[F[_]] {
    def map[A, B](t: F[A])(f: A => B): F[B]
  }

  /***
    *
    * Applicative trait uses map2 and unit as it's base methods
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
  }


  def main(args: Array[String]) : Unit = {

  }

}
