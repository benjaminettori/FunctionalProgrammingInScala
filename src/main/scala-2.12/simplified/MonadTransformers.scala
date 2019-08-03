package simplified

object MonadTransformers {

  import cats.effect.IO

  // Monad uses higher kinded types because monad type needs to be parametrized in order to apply map, flatMap
  trait Monad[F[_]] {
    def flatMap[A, B](obj: F[A])(f: A => F[B]): F[B]
    def map[A, B](obj: F[A])(f: A => B) : F[B] = flatMap(obj)(a => lift(f(a)))
    def lift[A](a: => A) : F[A]
  }


  class OptionMonad extends Monad[Option] {
    override def flatMap[A, B](obj: Option[A])(f: A => Option[B]): Option[B] = obj match {
      case Some(a) => f(a)
      case None => None
    }

    override def lift[A](a: => A): Option[A] = Option(a)
  }

  class IoMonad extends Monad[IO] {
    override def flatMap[A, B](obj: IO[A])(f: A => IO[B]): IO[B] = obj.flatMap(f)

    override def lift[A](a: => A): IO[A] = IO(a)
  }

  case class StateT[S, A, M[_]](run: S => M[(A, S)]) {
    def flatMap[B](f: A => StateT[S, B, M])(implicit M: Monad[M]) : StateT[S, B, M] = StateT { s: S => {
      val m = run(s)
      M.flatMap(m: M[(A, S)]) { as => f(as._1).run(as._2)}
    }}

    def map[B](f: A => B)(implicit M: Monad[M]) : StateT[S, B, M] = flatMap { a =>
      val b = f(a)
      StateT[S, B, M] {s => M.lift((b, s))}
    } (M)
  }

  object StateT {
    def lift[S, A, M[_]](mon: M[A])(implicit M : Monad[M]) : StateT[S, A, M] = StateT { currentState: S => M.flatMap(mon)(a => M.lift((a, currentState)))}
  }


  case class IntState(i: Int)

  def add(i: Int) = StateT[IntState, Int, IO] {oldState =>
    val newVal = oldState.i + i
    val newState = oldState.copy(i = newVal)
    IO((newVal, newState))
  }

  def multiply(i: Int) = StateT[IntState, Int, IO] { oldState =>
    val newVal = i * oldState.i
    val newState = oldState.copy(i = newVal)
    IO((newVal, newState))
  }

  implicit val ioMonad = new IoMonad

  // define a set of operations that will be applied to an input state
  val forExpression = for {
    _ <- add(1)
    _ <- add(4)
    x <- multiply(10)
  } yield x

  def getLine(): IO[String] = IO(scala.io.StdIn.readLine())
  def putStr(s: String) : IO[Unit] = IO(print(s))

  def toInt(s: String): Int = {
    try {
      s.toInt
    } catch {
      case _: NumberFormatException => 0
    }
  }

  case class SumState(s: Int)

  def updateAppState(newValue: Int) : StateT[SumState, Int, IO] = StateT { s: SumState =>
    val nextValue = newValue + s.s
    val newState = SumState(nextValue)
    IO((nextValue, newState))
  }


  // Lifting IO into StateT

  def liftIoIntoStateT[A](io: IO[A]) : StateT[SumState, A, IO] = StateT.lift(io)
  def liftAIntoStateT[A](a: A) : StateT[SumState, A, IO] = StateT.lift(IO(a))

  def getLineAsStateT(): StateT[SumState, String, IO] = liftIoIntoStateT(getLine())
  def putStrAsStateT(s: String): StateT[SumState, Unit, IO] = liftIoIntoStateT(putStr(s))


  // The point of state transformer is to be able to combine state with
  // another monad, such as an effects monad like IO
  // Since we can map both of these to a type as parameters, we can use them together

  // The idea behind StateT or any other monadic transformer is to "lift" some monad and state together
  // into another type that is a monad
  // That way none of the advantages of being a monad are lost during the lifting
  def main(args: Array[String]) = {
    val b = add(1)
    val c = b.run(IntState(1))
    val result = c.unsafeRunSync()
    println(result._1)

    val forResult : IO[(Int, IntState)] = forExpression.run(IntState(1))
    forResult.map(x => println(x)).unsafeRunSync()

    // StateT for comprehension
    val meta = for {
      _ <- putStrAsStateT("\n please enter number \n")
      input <- getLineAsStateT()
      num <- liftIoIntoStateT(IO(toInt(input)))
      sum <- updateAppState(num)
    } yield sum

    val resultMeta = meta.run(SumState(4))
    println(resultMeta.unsafeRunSync())
  }

}
