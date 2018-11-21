import Chapter11._
import Chapter13.ConsoleIo.ConsoleIoImpl.ConsoleIo
import Chapter7.Par

import scala.annotation.tailrec
import scala.io.StdIn

object Chapter13 {
  case class Player(name: String, score: Int)

  def contest(p1: Player, p2: Player) : Unit = {
    if(p1.score > p2.score)
      println(s"${p1.name} is the winner")
    else
      println(s"${p2.name} is the winner")
  }

  // Isolate logic to calculate winner
  def winner(p1: Player, p2: Player) : Option[Player] = {
    if(p1.score > p2.score) Some(p1)
    else if (p2.score > p1.score) Some(p2)
    else None
  }

  def contestRefactor(p1: Player, p2: Player) : Unit = {
    winner(p1, p2) match {
      case Some(Player(name, _)) => println(s"$name is the winner")
      case None => println("It's a tie")
    }
  }

  // Isolate message to produce logic
  def getMessage(result: Option[Player]) : String = {
    result match {
      case Some(Player(name, _)) => s"$name is the winner"
      case None => "It's a tie"
    }
  }

  // Functional refactor, isolate side effet
  def contestRe(p1 : Player, p2: Player) : Unit = {
    println(getMessage(winner(p1, p2)))
  }

  trait SimpleIO { self =>
    def run: Unit
    def ++(io: SimpleIO) : SimpleIO = new SimpleIO {
      def run = {
        self.run
        io.run
      }
    }
  }

  object SimpleIO {
    def empty : SimpleIO = new SimpleIO {
      override def run: Unit = ()
    }
  }

  def PrintLine(msg: String) : SimpleIO = {
    // anonymous class instantiation
    new SimpleIO {
      override def run: Unit = println(msg)
    }
  }

  def contestIO(p1: Player, p2: Player) : SimpleIO = {
    PrintLine(getMessage(winner(p1, p2)))
  }

  sealed trait TailRec[A] {
    def map[B](f : A => B) : TailRec[B] = flatMap(f andThen(x => Return(x)))
    def flatMap[B](f : A => TailRec[B]) : TailRec[B] = FlatMap(this, f)
  }

  // Class implementations of TailRec
  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A, B](ioa: TailRec[A], f: A => TailRec[B]) extends TailRec[B]

  object TailRec extends Mon[TailRec] {
    override def flatMap[A, B](fa: TailRec[A])(f: A => TailRec[B]): TailRec[B] = fa flatMap f
    override def unit[A](a: => A): TailRec[A] = new TailRec[A] {

    }
    def apply[A](a: => A): TailRec[A] = unit(a)

    @tailrec
    final def run[A](ioa :TailRec[A]) : A = ioa match {
      case Return(v) => v
      case Suspend(res) => res()
      case FlatMap(x, f: (Any => TailRec[A])) => x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g : (Any => TailRec[Any])) => run(y flatMap (a =>  g(a) flatMap f))
      }
    }
  }

  def fahrenheitToCelsius(f: Double) : Double = (f - 32) * 5.0/9.0

  def ReadLine : TailRec[String] = TailRec {scala.io.StdIn.readLine}
  def PrintLn(msg: String) : TailRec[Unit] = TailRec {println(msg)}

  // This for comprehension works because the Monad is IO
  def converter : TailRec[Unit] = for {
    _ <- PrintLn("Enter a temperatures in Fahrenheit: ")
    temp <- ReadLine.map(_.toDouble)
    _ <- PrintLn(fahrenheitToCelsius(temp).toString)
  } yield ()

  def printline(s: String) : TailRec[Unit] = {
    Suspend(() => Return(println(s)))
  }

  object TailRecExamples {
    // Examples of stack overflow causing code
    val f: Int => Int = (x: Int) => x
    val g = List.fill(100000)(f).foldLeft(f)((x, y) => x compose y)

    // Examples of stack safe code

    val fSafe: Int => TailRec[Int] = (x: Int) => Return(x)
    val gSafe : Int => TailRec[Int] = List.fill(100000)(fSafe).foldLeft(fSafe) {
          // complicated way of telling it to suspend execution.
      (a : (Int => TailRec[Int]), b: (Int => TailRec[Int])) => (x : Int) => Suspend(() => ()).flatMap(_ => a(x).flatMap(b))
    }

  }

  // Need to be able to support concurrency
  object AsyncObj {
    sealed trait Async[A] {
      def flatMap[B](f: A => Async[B]) : Async[B] = {
        FlatMap(this, f)
      }

      def map[B](f: A => B) : Async[B] = {
        //flatMap(a => Return(f(a)))
        flatMap(f andThen (Return(_)))
      }
    }

    @tailrec
    def step[A](a: Async[A]) : Async[A] = a match {
      case FlatMap(FlatMap(x, g: (Any => Async[Any])), f: (Any => Async[A])) => step(g(x) flatMap(y => f(y)))
      case FlatMap(Return(x), f: (Any => Async[A])) => step(f(x))
      case _ => a
    }

    import Chapter7.Par

    def run[A](as: Async[A]) : Par[A] = step(as) match {
      case Return(a) => Par.unit(a)
      case Suspend(res) => Chapter7.flatMap(res)(a => run(Return(a)))
      case FlatMap(x, f: (Any => Async[A])) => x match {
        case Suspend(r) => Chapter7.flatMap(r)(a => run(f(a)))
        case _ => sys.error("Impossible, step already handles this")
      }
    }

    case class Return[A](a: A) extends Async[A]
    case class Suspend[A](resume: Chapter7.Par[A]) extends Async[A]
    case class FlatMap[A, B](el: Async[A], f: A => Async[B]) extends Async[B]
  }

  object AbstractObj {

    /***
      * Abstract trait containing previous concepts
      * @tparam F
      * @tparam A
      */
    // Ex 13.1
    sealed trait Free[F[_], A] {
      def flatMap[B](f: A => Free[F, B]) : Free[F, B] = FlatMap(this, f)
      def map[B](f: A => B) : Free[F, B] = {
        flatMap(f andThen (Return(_)))
      }
    }

    def freeMonad[F[_]]: Mon[({type f[A] = Free[F, A]})#f] = {
      new Mon[({type f[A] = Free[F, A]})#f] {
        override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa.flatMap(f)

        override def unit[A](a: => A): Free[F, A] = Return(a)
      }
    }

    case class Return[A, F[_]](a: A) extends Free[F, A]
    case class Suspend[A, F[_]](r: F[A]) extends Free[F, A]
    case class FlatMap[A, B, F[_]](el: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

    // Ex 13.2
    @tailrec
    def runTrampoline[A](a: Free[Function0, A]) : A = {
      a match {
        case Return(a) => a
        case Suspend(f: Function0[A]) => f.apply()
        case FlatMap(el : Free[Function0, Any], f: (Any => Free[Function0, A])) => el match {
          case Return(x) => runTrampoline(f(x))
          case Suspend(r) => runTrampoline(f(r()))
          case FlatMap(x, g: (Any => Free[Function0, Any])) => runTrampoline(x.flatMap(y => g(y).flatMap(f)))
        }
      }
    }

    //Ex 13.3
    def run[F[_], A](a: Free[F, A])(implicit Fm: Mon[F]) : F[A] = {
      step(a) match {
        case Return(a) => Fm.unit(a)
        case Suspend(r) => Fm.flatMap(r)(a => run(Return(a)))
        case FlatMap(el, f: (Any => Free[F, A])) => el match {
          case Suspend(r: F[Any]) => Fm.flatMap(r)(a => run(f(a)))
          case _ => sys.error("Already covered by step")
        }
      }
    }

    @tailrec
    def step[F[_], A](fa: Free[F, A]) : Free[F, A] = fa match {
      case FlatMap(FlatMap(e, g: (Any => Free[F, Any])), f: (Any => Free[F, A])) => step(e.flatMap(x => g(x) flatMap f))
      case FlatMap(Return(e), f: (Any => Free[F, A])) => step(f(e))
      case _ => fa
    }

    /***
      * Example of going from abstract trait Free to Async
      * @tparam A
      */
    type Async[A] = Free[Chapter7.Par, A]
  }

  object ConsoleIo {
    import AbstractObj._
    sealed trait Console[A] {
      def toPar: Chapter7.Par[A]
      def toThunk: () => A
    }

    case object ReadLine extends Console[Option[String]] {
      override def toPar: Par[Option[String]] = Chapter7.Par.lazyUnit(run)

      override def toThunk: () => Option[String] = () => run

      def run: Option[String] = {
        try Some(StdIn.readLine())
        catch {
          case _: Exception => None
        }
      }
    }

    case class PrintLine(line: String) extends Console[Unit] {
      override def toPar: Par[Unit] = Chapter7.Par.lazyUnit(println(line))

      override def toThunk: () => Unit = () => printline(line)
    }

    // Here we define a program that can interact with the console.
    // We defined F[_] as Console[_]. This type can implement a thunk or a Par
    // We are wrapping side effecting code in a structure of type Free
    // We can pass these objects without causing side effects
    // The run function isolates the side effects.

    // Note we need a Monad to actually run ConsoleIo[A]
    // But Console is not a monad.
    object ConsoleIoImpl {
      type ConsoleIo[A] = Free[Console, A]

      def readLn : ConsoleIo[Option[String]] = {
        AbstractObj.Suspend(ReadLine)
      }

      def printLn(line: String): ConsoleIo[Unit] = {
        AbstractObj.Suspend(PrintLine(line))
      }
    }

    trait Translate[F[_], G[_]] {
      def apply[A](f: F[A]) : G[A]
    }

    //For trait Category[~>[_, _]], ~> is just the placeholder-name for the type-parameter of Category. Like the T in class Option[T].
    //
    //Additionally, Scala syntax allows you to write B ~> C as a shorthand for ~>[B, C].
    type ~>[F[_], G[_]] = Translate[F, G]


    // Now we can convert Console to a Monad type.
    val consoleToFunc : Console ~> Function0 = new (Console ~> Function0) {
      override def apply[A](f: Console[A]): () => A = f.toThunk
    }

    val consoleToPar = new (Console ~> Chapter7.Par) {
      override def apply[A](f: Console[A]): Par[A] = f.toPar
    }

    case class Return[A, F[_]](a: A) extends Free[F, A]
    case class Suspend[A, F[_]](r: F[A]) extends Free[F, A]
    case class FlatMap[A, B, F[_]](el: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

    @tailrec
    def step[F[_], A](fa: Free[F, A]) : Free[F, A] = fa match {
      case FlatMap(FlatMap(e, g: (Any => Free[F, Any])), f: (Any => Free[F, A])) => step(e.flatMap(x => g(x) flatMap f))
      case FlatMap(Return(e), f: (Any => Free[F, A])) => step(f(e))
      case _ => fa
    }

    // Run method can take types that do not have a Monad, as long as a converter exists to a type with a Monad
    def run[F[_], G[_], A](c: Free[F, A])(t: F ~> G)(implicit mg: Mon[G]) : G[A] = step(c) match {
      case Return(a) => mg.unit(a)
      case Suspend(r) => t.apply(r)
      case FlatMap(Suspend(r), f: (Any => Free[F, A])) => mg.flatMap(t.apply(r))(x => run(f(x))(t))
      case _ => sys.error("Already covered in step method")
    }

    // In order to run Console to a function or Par, need implicitly defined Monads
    // Then can use the run functio and the converters.
    implicit val function0Monad = new Mon[Function0] {
      override def unit[A](a: => A): () => A = () => a

      override def flatMap[A, B](fa: () => A)(f: A => () => B): () => B = () => f(fa())()
    }

    implicit val parMonad = new Mon[Par] {
      override def unit[A](a: => A): Par[A] = Par.lazyUnit(a)

      override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = Par.fork{ Chapter7.flatMap(fa)(f) }
    }

    def runFunction0[A](c: Free[Console, A]) : () => A = {
      run[Console, Function0, A](c)(consoleToFunc)
    }

    def runPar[A](c: Free[Console, A]) : Par[A] = {
      run[Console, Par, A](c)(consoleToPar)
    }
  }

  def main(args: Array[String]): Unit = {
    println("Test")
  }


}
