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

    /***
      *
      * Ex 11.3
      */
    def sequence[A](lma: List[F[A]]) : F[List[A]] = lma match {
      case fa :: h => map2(fa, sequence(h))(_ :: _)
      case fa :: Nil => flatMap(fa)(a => unit(List(a)))
      case Nil => unit(Nil)
    }

    def traverse[A, B](la: List[A])(f: A => F[B]) : F[List[B]] = sequence(la.map(a => f(a)))

    /***
      *
      * Ex 11.4
      */

    def replicateM[A](n: Int, ma: F[A]) : F[List[A]] = sequence(List.fill(n)(ma))


    def product[A, B](ma: F[A], mb: F[B]) : F[(A, B)] = map2(ma, mb)((_, _))
    /***
      * Ex 11.6
      */

    def filterM[A](ms: List[A])(f: A => F[Boolean]) : F[List[A]] = {
     ms match {
       case Nil => unit(Nil)
       case h :: t => flatMap(f(h))(b => {
         if(!b) filterM(t)(f)
         else map(filterM(t)(f))(h :: _)
       })
     }
    }

    def compose[G[_]](G: Mon[G]): Mon[({type f[x] = F[G[x]]})#f] = {
      val self = this

      new  Mon[({type f[x] = F[G[x]]})#f] {
        override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

        // this will not work because the function f returns F[G[B] which is not the argunment needed for G.flatmap.
        override def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] = ???
      }
    }

    /***
      * Ex 11.7
      * Kleisli composition.
      * This can be used to demonstrate the associativity law of the monad
      * compose(compose(f, g), h) == compose(f, compose(g, h))
      */

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

    /***
      * Ex 11.8
      * Implement flatMap in terms of compose
      */

    def flatMapCompose[A, B](fa: F[A])(f: A => F[B]) : F[B] = compose((_: Unit) => fa, f)()

    /***
      * Ex 11.9
      * We can prove that flatMap associative law is equivalent to compose associative law
      *
      * x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
      *
      * compose(f, g) == x => f(x).flatMap(g)
      * compose(f, compose(g, h)) == x => f(x).flatMap(compose(g, h)) == x => f(x).flatMap(y => g(y).flatMap(h)) == x => f(x).flatMap(g).flatMap(h)
      * compose(compose(f,g), h) == x => f(x).flatMap(g).flatMap(h) == compose(f, compose(g, h))
      */

    /***
      * Ex 11.10
      *
      * Monad identity law
      *
      * compose(f, unit) == f
      * compose(unit, f) == f
      *
      * flatMap(x)(unit) == x
      * flatMap(unit(y))(f) == f(y)
      */

    /***
      * Ex 11.11
      *
      * Monad identity law for List monad
      *
      * compose(f, a => List(a)) = x => f(x).flatMap(a => List(a)) = x => List(f(x))
      */

    /***
      * Ex 11.12
      *
      */

    def join[A](mma: F[F[A]]) : F[A] = flatMap(mma)(a => a)

    /***
      * Ex 11.13
      *
      */

    def flatMapJoin[A, B](ma : F[A])(f: A => F[B]) : F[B] = join(map(ma)(a => f(a)))

    def composeJoin[A, B, C](f: A => F[B], g: B => F[C]) : A => F[C] = a => join(map(f(a))(b => g(b)))

    /***
      * Ex 11.14
      *
      * Monad laws with join, map, unit
      *
      * Associativity
      *
      * Identity
      *
      * compose(f, unit) = compose(unit, f) = f
      * a => join(map(f(a))(b => unit(b))) == a => join(map(unit(a))(a => f(a))) == f
      */

    /***
      * Ex 11.15
      *
      * It does not matter in what order the Par and Parser constructs are applied
      */

    /***
      * Ex 11.16
      *
      * identity laws for List
      */

    /***
      * Formal definition of a monad
      *
      * There are 3 primitive sets of Monad combinators
      *
      * unit and flatMap
      * unit and compose
      * unit, map and join
      *
      * A monad is an implementation of one of these sets of combinators that satifies the associativity and identity lawss
      */
  }

  /***
    * Ex 11.1
    */

  // List is a Monad

  val listMonad = new Mon[List] {
    def unit[A](a: => A) : List[A] = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]) : List[B] = fa.flatMap(a => f(a))
  }

  val optionMonad = new Mon[Option] {
    def unit[A](a: => A) : Option[A] = Some(a)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]) : Option[B] = fa.flatMap(a => f(a))
  }

  val streamMonad = new Mon[Stream] {
    def unit[A](a: => A): Stream[A] = a #:: Stream.empty

    override def flatMap[A, B](fa: Stream[A])(f: (A) => Stream[B]): Stream[B] = fa.flatMap(a => f(a))
  }

  /***
    * Ex 11.2
    */

  class Monad[S] {
    type StateS[A] = Chapter6.ScalaState[S, A]

    val stateMonad = new Mon[StateS] {
      def unit[A](a: => A) : StateS[A] = Chapter6.ScalaState(s => (a, s))
      override def flatMap[A, B](fa: StateS[A])(f: (A) => StateS[B]): StateS[B] = fa.flatMap(a => f(a))
    }
  }

  // If don't want to use class, use inline notation instead
  // # operator allows us to access members of the anonymous type we created between parentheses
  def stateMonad[S] = new Mon[({type f[x] = Chapter6.ScalaState[S, x]})#f] {
    def unit[A](a: => A) : Chapter6.ScalaState[S, A] = Chapter6.ScalaState(s => (a,s))
    override def flatMap[A, B](fa: Chapter6.ScalaState[S, A])(f: (A) => Chapter6.ScalaState[S, B]): Chapter6.ScalaState[S, B] = fa.flatMap(a => f(a))
  }

  /***
    * Ex 11.17
    * Monad for Id class
    */

  case class Id[A](value: A) {
    def map[B](f: A => B) : Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }


  val idMonad = new Mon[Id] {
    override def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]) = fa.flatMap(f)

    override def map[A, B](fa: Id[A])(f: (A) => B): Id[B] = fa.map(f)

    override def unit[A](a: => A) = Id(a)
  }


  case class Reader[R, A](run: R => A) {
    def flatMap[B](f: A => Reader[R, B]) : Reader[R, B] = Reader(r => f(run(r)).run(r))
    def map[B](f: A => B): Reader[R, B] = Reader(r => f(run(r)))
  }
  object Reader {
    def readerMonad[R] = new Mon[({type f[x] = Reader[R, x]})#f] {
      override def unit[A](a: => A) = Reader(_ => a)

      override def flatMap[A, B](fa: Reader[R, A])(f: (A) => Reader[R, B]) = fa.flatMap(f)

      override def map[A, B](fa: Reader[R, A])(f: (A) => B): Reader[R, B] = fa.map(f)
    }
  }

  def main(args: Array[String]): Unit = {
    /***
      * Ex 11.5
      */
    val listMonad = new Mon[List] {
      def unit[A](a: => A) : List[A] = List(a)
      def flatMap[A, B](fa: List[A])(f: A => List[B]) : List[B] = fa.flatMap(a => f(a))
    }

    val optionMonad = new Mon[Option] {
      def unit[A](a: => A) : Option[A] = Some(a)
      def flatMap[A, B](fa: Option[A])(f: A => Option[B]) : Option[B] = fa.flatMap(a => f(a))
    }

    val listReplicate = listMonad.replicateM(3, List(3, 5))
    println(listReplicate)
    println(List.fill(3)(List(3, 5)))

    val optionReplicate = optionMonad.replicateM(3, Some("value"))
    println(optionReplicate)

    /***
      * We can use the id monad to illustrate the purpose of a Monad.
      *
      * The idea is to use a Monad for variable substitution, and applying operations to the variables
      *
      */

    val test = for {
      a <- Id("Hello")
      b <- Id(", world")
    } yield a + b

    println(test)

    /***
      * In the code above, we wrap the strings in the Id construct and substitute variables. Since Id is a Monad, we can apply the
      * for comprehension through the use of map and flatMap
      *
      */

    /***
      * Ex 11.18
      * Uses of state monad
      */

    val F = stateMonad[Int]

    val stateRep = F.replicateM(2, Chapter6.ScalaState(x => ("Hello ", x)))
    println(stateRep.run(2))

    val stateMap2 = F.map2(Chapter6.ScalaState(x => (4, x)), Chapter6.ScalaState(x => (5, x)))((x, y) => x + y)
    println(stateMap2.run(4))

    /***
      * Ex 11.19
      *
      * getState and setState
      */

     val intState = Chapter6.ScalaState[Int, Int](x => (4, x))

    // this is the same as F.unit(())
    val unitTest = Chapter6.ScalaState.get[Int].flatMap(x => Chapter6.ScalaState.set(x))

    // Same as above
    for {
      a <- Chapter6.ScalaState.get[Int]
      _ <- Chapter6.ScalaState.set(a)
    } yield ()


    val stateResult = for {
      _ <- Chapter6.ScalaState.set(1)
      b <- Chapter6.ScalaState.get[Int]
    } yield b

    println(stateResult.run(1))

    /***
      * Example code
      *
      * State starts at 0, for each pass of loop incremented by 1 and reset
      * This gives us the index for each element in the list
      * The state monad specifies that the updated state will then be available to the next iteration
      */

    def zipWithIndex[A](as: List[A]) : List[(Int, A)] = as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
      x <- acc
      b <- Chapter6.ScalaState.get[Int]
      _ <- Chapter6.ScalaState.set(b + 1)
    } yield (b, a) :: x).run(0)._1.reverse

    /***
      * Ex 11.20
      * Reader Monad
      *
      * replicate method will apply the same run method n times to same input r, producing a list of results
      * sequence will take a list of readers, and apply the run method of each reader in succession to the input, producing a list of results.
      */

    val reader = Reader((r: String) => r.length)
    val rep = Reader.readerMonad.replicateM(3, reader)
    println(rep.run("test"))

    val reader2 = Reader((r: String) => r.length - 1)

    val seqRes = Reader.readerMonad.sequence(List(reader, reader2))
    println(seqRes.run("test"))

  }
}
