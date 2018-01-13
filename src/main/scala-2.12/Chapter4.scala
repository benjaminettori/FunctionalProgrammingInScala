/**
  * Created by bettori on 8/30/2017.
  */
object Chapter4 {

  // Ex 4.1
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A] = {
       this match {
         case Some(a) => if(f(a)) Some(a) else None
         case None => None
       }
    }
  }
  case class Some[+A](get: A) extends Option[A] {
    def map[B](f: A => B): Option[B] = {
      Some(f(get))
    }

    override def flatMap[B](f: (A) => Option[B]) = {
      f(get)
    }

    override def getOrElse[B >: A](default: => B) = get

    override def orElse[B >: A](ob: => Option[B]) = {
      Some(get)
    }
  }

  case object None extends Option[Nothing] {
    def map[B](f: Nothing => B): Option[Nothing] = {
      None
    }

    override def flatMap[B](f: (Nothing) => Option[B]) = None

    override def getOrElse[B >: Nothing](default: => B) = default

    override def orElse[B >: Nothing](ob: => Option[B]) = ob
  }

  // Ex 4.2
  def mean(xs: Seq[Double]): Option[Double] = {
    val seqSize = xs.length
    seqSize match {
      case 0 => None
      case l => Some(xs.sum / l)
    }
  }

  def variance(xs: Seq[Double]) : Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(v => math.pow(v - m, 2))))
  }

  // Unfamiliar syntax
  // _ seems to be Option[A]
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  // here _ is Seq[A]
  def test[A, B](f: A => B): Seq[A] => Seq[Boolean] = _ map (_ => true)

  // Ex 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (Some(x), Some(y)) => Some(f(x,y))
      case (_, _) => None
    }
  }

  // Ex 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap(hh => sequence(t) map (hh :: _))
    }
  }

  // Ex 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))((x, y) => x :: y)
    }
  }

  // Ex 4.6
  trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = {
      this match {
        case Right(a) => Right(f(a))
        case Left(e) => Left(e)
      }
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]) : Either[EE, B] = {
      this match {
        case Right(a) => f(a)
        case Left(l) => Left(l)
      }
    }

    def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(_) => b
        case Right(r) => Right(r)
      }
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C) : Either[EE, C] = {
      (this, b) match {
        case (Right(r), Right(e)) => Right(f(r, e))
        case (Left(l), _) => Left(l)
        case (_, Left(l)) => Left(l)
      }
    }

    // Ex 4.7
    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      as match {
        case Nil => Right(Nil)
        case h :: t => traverse(t)(f).map2(f(h))((a, b) => b :: a)
      }
    }

    def sequence[E, A](es : List[Either[E, A]]): Either[E, List[A]] = {
      traverse(es)(a => a)
    }
  }

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {case e: Exception => Left(e)}
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int) : Double = {
    age + 0.5 * numberOfSpeedingTickets
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] = {
    for {
      a <- Try(age.toInt)
      tickets <- Try(numberOfSpeedingTickets.toInt)
    } yield insuranceRateQuote(a, tickets)
  }

  case class Left[+E](value: E) extends Either[E, Nothing] {}

  case class Right[+A](value: A) extends Either[Nothing, A] {}

  // Example of using map2

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] = {
    if(name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))
  }

  def mkAge(age: Int): Either[String, Age] = {
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))
  }

  // map2 is used to combine age and name, extracting value from Either object, to make a person object
  def mkPerson(name: String, age: Int): Either[String, Person] = {
    mkAge(age).map2(mkName(name))((a, n) => Person(n, a))
  }

  // Ex 4.8
  // Can take a list of strings or list of exceptions as error type
  def example() : Either[List[String], Int] = ???

  // Can also redefine either
  trait EitherNew[List[+E], +A]

  def main(args: Array[String]): Unit = {
    val test = Some[Int](1)
    val pTest = test.flatMap[Int](x => Some(x))
    println(pTest)

    // testing the for comprehension with Either
    println(parseInsuranceRateQuote("23", "hello"))
    println(parseInsuranceRateQuote("23", "1"))

    val eitherTest = Right(2)
    println(eitherTest.sequence(List(Right(1), Right(2), Right(3), Right(4), Right(5))))
    println(eitherTest.sequence(List(Right(1), Right(2), Left(3), Left(4), Right(5))))
  }

}
