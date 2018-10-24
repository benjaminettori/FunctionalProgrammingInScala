import Chapter11.Mon

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

  trait IO[A] { self =>
    def run : A
    def map[B](f : A => B) : IO[B] = new IO[B] {
      def run : B = f(self.run)
    }
    def flatMap[B](f : A => IO[B]) : IO[B] = f(self.run)
  }

  object IO extends Mon[IO] {
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
    override def unit[A](a: => A): IO[A] = new IO[A] {
      override def run: A = a
    }
    def apply[A](a: => A): IO[A] = unit(a)
  }

  def fahrenheitToCelsius(f: Double) : Double = (f - 32) * 5.0/9.0

  def ReadLine : IO[String] = IO {scala.io.StdIn.readLine}
  def PrintLn(msg: String) : IO[Unit] = IO {println(msg)}

  // This for comprehension works because the Monad is IO
  def converter : IO[Unit] = for {
    _ <- PrintLn("Enter a temperatures in Fahrenheit: ")
    temp <- ReadLine.map(_.toDouble)
    _ <- PrintLn(fahrenheitToCelsius(temp).toString)
  } yield ()
}
