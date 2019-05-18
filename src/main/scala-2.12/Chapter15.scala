object Chapter15 {

  // Process specifies transformation from one stream to another
  // Stream[I] into Stream[O]
  sealed trait Process[I,O] {
    def apply(s: Stream[I]) : Stream[O] = {
      this match {
        case Halt() => Stream()
        case Await(receive) => s match {
          case hs #:: ts => receive(Some(hs))(ts)
          case xs => receive(None)(xs)
        }
        case Emit(h : O, t: Process[I, O]) => h #:: t(s)
      }
    }

    // apply method above only processes one element
    // The repeat method is a recursive call to process multiple elements
    // If it sees a Halt, it starts over go(this) and processes the tail of the Stream

    // This function only works when used with an Await as it's entry point.
    // A Halt or Emit will probably cause an infinite loop
    // Await is entry point. Alternate between Emit and Await until reach end of stream
    def repeat: Process[I, O] = {
      def go(process: Process[I, O]) : Process[I, O] = {
        process match {
          case Halt() => go(this)
          case Await(receive) => Await {
            case None => receive(None)
            case i => go(receive(i))
          }
          case Emit(h, t) => Emit(h, go(t))
        }
      }

      go(this)
    }
  }

  case class Halt[I,O]() extends Process[I,O]

  case class Await[I, O](receive: Option[I] => Process[I, O]) extends Process[I, O]

  case class Emit[I, O](head: O, tail: Process[I,O] = Halt[I, O]()) extends Process[I, O]

  // Code takes any function from I => O and returns a Process[I, O]
  // Emits I value to O Stream
  def liftOne[I, O](liftFunction: I => O) : Process[I, O] = Await {
    case Some(i) => Emit(liftFunction(i))
    case None => Halt()
  }


  // Class[A : B] defines a context bound B on A, ie an implicit variable of type B[A]

  def lift[I, O](liftFunction: I => O) : Process[I, O] = liftOne(liftFunction).repeat

  def filter[I](pred: I => Boolean) : Process[I, I] = {
    Await[I, I] {
      case Some(i) if pred(i) => Emit(i)
      case _ => Halt()
    }.repeat
  }

  def sum : Process[Double, Double] = {
    def go(accSum: Double) : Process[Double, Double] = Await[Double, Double] {
      case Some(d) => Emit(d + accSum, go(accSum + d))
      case _ => Halt()
    }
    go(0.0)
  }

  // Ex 15.1
  def take[I](n: Int) : Process[I, I] = Await {
    case Some(i) if n > 0 => Emit(i, take(n-1))
    case _ => Halt()
  }

  def drop[I](n: Int) : Process[I, I] = Await {
    case Some(_) if n > 0 => drop(n-1)
    case Some(i) => Emit(i, drop(n-1))
    case _ => Halt()
  }

  def takeWhile[I](predicate: I => Boolean) : Process[I, I] = Await {
    case Some(i) if predicate(i) => Emit(i, takeWhile(predicate))
    case _ => Halt()
  }

  def dropWhile[I](predicate: I => Boolean) : Process[I, I] = Await {
    case Some(i) => if(predicate(i)) dropWhile(predicate) else Emit(i, lift((x: I) => x))
    case _ => Halt()
  }

  def main(args: Array[String]) : Unit = {


    // Example of how to initiate processing of stream
    // lift wraps the function we wish to apply to stream in Process
    // Then invoke apply method of Process to parse Stream and apply method

    val lifted = liftOne((x:Int) => x * 2)
    val xs = lifted(Stream(1, 2, 3)).toList
    val fullStream = lift((x: Int) => x * 2)
    println(xs)
    println(fullStream(Stream(1, 2, 3)).toList)


    val evens = filter((x: Int) => x % 2 == 0)
    val result = evens(Stream(1,2,3,4)).toList
    println(result)

    // sum
    val sumResult = sum(Stream(1.0,2.0,3.0,5.0))
    println(sumResult.toList)

    // take
    val wordStream = Stream("hello", "my", "name", "is", "Mud")
    val takeResult = take(3)(wordStream)
    println(takeResult.toList)

    // drop
    val dropResult = drop(2)(wordStream)
    println(dropResult.toList)

    val numberStream = Stream(1, 2.5, 3.5, 6, 2, 100)
    // takeWhile
    val takeWhileResult = takeWhile((x: Double) => x < 4)(numberStream)
    println(takeWhileResult.toList)

    // dropWhile
    val dropWhileResult = dropWhile((x: Double) => x < 4)(numberStream)
    println(dropWhileResult.toList)
  }


}
