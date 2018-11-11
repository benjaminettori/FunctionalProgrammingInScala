import Chapter11._

import scala.annotation.tailrec

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

  def main(args: Array[String]): Unit = {
    println("Test")
  }


}
