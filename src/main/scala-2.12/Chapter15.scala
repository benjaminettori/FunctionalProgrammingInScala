import java.io.{File, FileWriter}

import Chapter11.Mon
import cats.effect.IO
import Chapter15.Extensible.Process.{End, Process, linesExample}



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

    // Ex 15.5
    // Pipe two processes together
    def |>[O2](p2: Process[O, O2]) : Process[I, O2] = {
      p2 match {
        case Halt() => Halt()
        case Emit(h, t) => Emit(h, this.|>(t))
        case Await(f) => this match {
          case Halt() => Halt() |> f(None)
          case Emit(h, t) => t |> f(Some(h))
          case Await(h) => Await((x : Option[I]) => h(x) |> p2)
        }
      }
    }

    def map[O2](f: O => O2) : Process[I, O2] = this |> lift(f)

    def ++(p: Process[I, O]) : Process[I, O] = this match {
      case Halt() => p
      case Emit(head, tail) => Emit(head, tail ++ p)
      case Await(receive) => Await(receive andThen(_ ++ p))
    }


    def flatMap[O2](f: O => Process[I, O2]) : Process[I, O2] = this match {
      case Halt() => Halt()
      case Emit(head, tail) => f(head) ++ tail.flatMap(f)
      case Await(receive) => Await(receive andThen(_.flatMap(f)))
    }

    // Ex 15.6
    // Zip output of process with index
    def zipWithIndex : Process[I, (Int, O)] = {
      def count(currentCount: Int, currentProcess: Process[I, O]) : Process[I, (Int, O)] = {
        currentProcess match {
          case Halt() => Halt()
          case Emit(head: O, tail: Process[I, O]) => Emit((currentCount, head), count(currentCount + 1, tail))
          case Await(receive) => Await ((optI : Option[I]) => count(currentCount, receive(optI)))
        }
      }

      count(0, this)
    }

    // Ex 15.7
    // Generic combinator
    def combineWithOperation[B, C](combineProcess: Process[I, B], combineOperation: (O, B) => C) : Process[I, C] = {
      def feed[U](optionI : Option[I], process : Process[I, U]) : Process[I, U] = {
        process match {
          case Halt() => Halt()
          case Emit(h, t) => Emit(h, feed(optionI, t))
          case Await(receives) => receives(optionI)
        }
      }

      (this, combineProcess) match {
        case (Halt(), _) => Halt()
        case (_, Halt()) => Halt()
        case (Emit(head: O, tail: Process[I, O]), Emit(headB: B, tailB: Process[I, B])) => Emit(combineOperation(head, headB), tail.combineWithOperation(tailB, combineOperation))
        case (_, Await(receive)) => Await((iOpt: Option[I]) => feed(iOpt,this).combineWithOperation(receive(iOpt), combineOperation))
        case (Await(receive), _) => Await((iOpt: Option[I]) => receive(iOpt).combineWithOperation(feed(iOpt, combineProcess), combineOperation))
      }
    }
  }


  def monad[I] : Mon[({type f[x] = Process[I, x]})#f] =
    new Mon[({type f[x] = Process[I, x]})#f] {
      def unit[O](o: => O): Process[I, O] = Emit(o)
      def flatMap[O, O2](p: Process[I, O])(f : O => Process[I, O2]) : Process[I, O2] = p flatMap f
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

  // Ex 15.2
  def count[I]: Process[I, Int] = {
    def countRecurse(acc: Int) : Process[I, Int] = Await {
      case Some(_) => Emit(acc, countRecurse(acc + 1))
      case _ => Halt()
    }

    countRecurse(1)
  }

  // Ex 15.3
  def mean: Process[Double, Double] = {
    def runningAvg(sum: Double, count: Int) : Process[Double, Double] = Await {
      case Some(i) => Emit((sum + i) / (count + 1), runningAvg(sum + i, count + 1))
      case _ => Halt()
    }

    runningAvg(0, 0)
  }

  def emit[I, O](head: O, tail: Process[I, O]) : Process[I, O] = {
    Emit(head, tail)
  }

  def await[I, O](f: I => Process[I, O], fallback: Process[I, O] = Halt[I, O]()) : Process[I, O] = Await {
    case Some(i) => f(i)
    case _ => fallback
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)) : Process[I, O] = await((i: I) => {
    f(i, z) match {
      case (o, s2) => emit(o, loop(s2)(f))
    }
  })

  // Ex 15.4

  def sumLoop: Process[Double, Double] ={
    loop(0.0)((next: Double, acc: Double) => {
      (next + acc, next + acc)
    })
  }

  def countLoop[I] : Process[I, Int] = loop(0)((next: I, currentCount: Int) => {
    (currentCount + 1, currentCount + 1)
  })

  // Ex 15.8
  // exists determines if predicate condition is met in stream
  def exists[I](predicate: I => Boolean): Process[I, Boolean] = {
    def getFinalResult(currentResult: Boolean) : Process[I, Boolean] =  Await {
      case None => Emit(currentResult)
      case Some(i) => getFinalResult(currentResult || predicate(i))
    }
    getFinalResult(false)
  }

  def processFile[A, B](p: Process[String, A], z: B)(g: (B, A) => B)(f: java.io.File)= {
    def go(ss: Iterator[String], cur: Process[String, A], acc: B) : B = cur match {
      case Halt() => acc
      case Await(receive) => val next = if (ss.hasNext) receive(Some(ss.next)) else receive(None)
        go(ss, next, acc)
      case Emit(h, t) => go(ss, t, g(acc, h))
    }

    val s = io.Source.fromFile(f)
    val lines = s.getLines()
    go(lines, p, z)
  }

  // Ex 15.9
  // convert list of temp values from Fahrenheit to Celsius
  def toCelsius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32.0)

  def getDouble: Process[String, Double] =
    Await {
    case Some(s) => if(s.isEmpty || s.startsWith("#")) getDouble else Emit(s.toDouble, getDouble)
    case None => Halt()
  }

  def getCelsius = processFile(getDouble |> lift(toCelsius), List[Double]())((l, c) => l :+ c)_

  object Extensible {

    // This process type is more general
    // Can take any type of input, doesn't assume a Stream
    // Produces a stream of output type O

    object Process {

      trait Process[F[_], O] {
        // How to handle the halt event
        def onHalt(f: Throwable => Process[F, O]) : Process[F, O] = this match {
          case Halt(e) => f(e)
          case Emit(h, t) => Emit(h, t.onHalt(f))
          case Await(req, recv) => Await(req, recv.andThen(_.onHalt(f)))
        }

        // Append method
        def ++(p: => Process[F, O]) : Process[F, O] = {
          this.onHalt {
            case End => p
            case err => Halt(err)
          }
        }

        def map[O2](f: O => O2): Process[F,O2] = this match {
          case Await(req,recv) =>
            Await(req, recv andThen (_ map f))
          case Emit(h, t) => Try { Emit(f(h), t map f) }
          case Halt(err) => Halt(err)
        }

        def filter(f: O => Boolean) : Process[F, O] = this |> Process.filter1(f).asInstanceOf[Process1[O, O]]


        def repeat: Process[F,O] = this ++ this.repeat

        def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = this match {
          case Halt(err) => Halt(err)
          case Emit(t, h) => Try(f(t)) ++ h.flatMap(f)
          case Await(ref, receive) => Await(ref, receive.andThen(_.flatMap(f)))
        }

        def runLog(implicit F : MonadCatch[F]) : F[IndexedSeq[O]] = {

          def go(curr : Process[F, O], acc : IndexedSeq[O]) : F[IndexedSeq[O]] = {
            curr match {
              case Emit(h, t) => go(t, acc :+ h)
              case Halt(End) => F.unit(acc)
              case Halt(err) => F.fail(err)
              case Await(req, recv: (Either[Throwable, Any] => Process[F, O])) => F.flatMap(F.attempt(req))(a => go(Try(recv(a)), acc))
            }
          }

          go(this, IndexedSeq())
        }


        // This method converts a process into
        // a process that invokes itself when given a kill signal
        // If a kill signal is received, we want to ensure recv will not cause the process to exit
        // before it can execute the next process it is linked to by onComplete
        // This means we can ignore the Kill signal and instead wait for the Halt
        // It will just keep reading from F until it gets to a Halt statement, usually triggered by an End exception
        // when it gets to the end of the "stream" it is reading from

        def asFinalizer: Process[F, O] = this match {
          case Emit(h, t) => Emit(h, t.asFinalizer)
          case Halt(err) => Halt(err)
          case Await(req, recv: (Either[Throwable, Any] => Process[F, O])) => await(req) {
            case Left(Kill) => this.asFinalizer
            case x => recv(x)
          }
        }

        // Method will ensure that even if this is killed
        // will still execute p, then propagate the error
        def onComplete(p: => Process[F, O]) : Process[F, O] = this.onHalt {
          case End => p.asFinalizer
          case err => p.asFinalizer ++ Halt(err)
        }

        // This method just "converts" to Process[F, O2] just by getting to Halt
        // which can pass for Process of any type
        def drain[O2]: Process[F,O2] = this match {
          case Halt(e) => Halt(e)
          case Emit(h, t : Process[F, O2]) => t.drain
          case Await(req,recv) => Await(req, recv andThen (_.drain))
        }

        def |>[O2](p2: Process1[O, O2]): Process[F, O2] = {
          p2 match {
            case Halt(e) => this.kill.onHalt(e2 => Halt(e) ++ Halt(e2))
            case Emit(h, t) => Emit(h, this |> t)
            case Await(req, recv: ((Either[Throwable, Any]) => Process1[Any, O])) => this match {
              case Halt(err) => Halt(err) |> recv(Left(err))
              case Emit(h1, t1) => t1 |> recv(Right(h1))
              case Await(req1, recv1 : ((Either[Throwable, Any]) => Process[F, O])) => await(req1)(recv1 andThen(_ |> p2))
            }
          }
        }

        def zipWith[O2, O3](p2: Process[F, O2])(f: (O, O2) => O3): Process[F, O3] = {
          val test : Process[T[O, O2]#f, O3] = Extensible.Process.zipWith(f).asInstanceOf[Process[T[O, O2]#f, O3]]
          this.tee(p2)(test)
        }

        def to[O2](sink: Sink[F, O]): Process[F, Unit] = join(this.zipWith(sink)((o, f) => f(o)))

        def pipe[O2](p2: Process1[O, O2]) : Process[F, O2] = this |> p2

        /***
          * Purpose of this method is to force terminate a process*
          */
        @annotation.tailrec
        final def kill[O2]: Process[F, O2] = this match {
          case Await(_, recv) => recv(Left(Kill)).drain.onHalt {
            case Kill => Halt(End)
            case e => Halt(e)
          }
          case Halt(e) => Halt(e)
          case Emit(h, t) => t.kill
        }

        def tee[O2, O3](p2: Process[F, O2])(t: Tee[O, O2, O3]) : Process[F, O3] = t match {
          case Halt(e) => (this.kill onComplete p2.kill[O3]).onComplete(Halt(e))
          case Emit(h, t) => Emit(h, this.tee(p2)(t))
          case Await(side, recv: (Either[Throwable, Any] => Process[F, O3])) => side.get match {
            case Left(isO) => this match {
              case Halt(e) => p2.kill[O3].onComplete(Halt(e))
              case Emit(o, ot) => ot.tee(p2)(Try(recv(Right(o))).asInstanceOf[Tee[O, O2, O3]])
              case Await(reqL, recvL: (Either[Throwable, Any] => Process[F, O])) => await(reqL)(recvL.andThen(_.tee(p2)(t)))
            }
            case Right(isO2) => p2 match {
              case Halt(e) => this.kill.onComplete(Halt(e))
              case Emit(o2, ot) => this.tee(ot)(Try(recv(Right(o2))).asInstanceOf[Tee[O, O2, O3]])
              case Await(reqR, recvR: (Either[Throwable, Any] => Process[F, O2])) => await(reqR)(recvR.andThen(this.tee(_)(t)))
            }
          }
        }
      }

      trait MonadCatch[F[_]] extends Mon[F] {
        def attempt[A](a: F[A]) : F[Either[Throwable, A]]
        def fail[A](t: Throwable) : F[A]
      }


      case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]
      case class Emit[F[_], O](head: O, tail: Process[F, O] = Halt(End)) extends Process[F, O]

      // Halt takes an exception to explain reason for halting (End or Kill)
      case class Halt[F[_], O](err: Throwable) extends Process[F, O]

      case object End extends Exception
      case object Kill extends Exception


      // Helper methods
      def Try[F[_], O](p: => Process[F, O]) : Process[F, O] = try p catch {
        case e: Throwable => Halt(e)
      }

      def await[F[_], A, O](req: F[A])(receive: Either[Throwable, A] => Process[F, O]) : Process[F, O] = Await(req, receive)
      def emit[F[_], O](h: O, t: Process[F, O] = Halt[F, O](End)): Process[F, O] = Emit(h, t)

      // Ex 15.11
      def eval[F[_], A](a: F[A]) : Process[F, A] = await(a) {
        case Left(err) => Halt(err)
        case Right(x) => Emit(x, Halt(End))
      }


      def eval_[F[_], A, B](a: F[A]) : Process[F, B] = eval[F, A](a).drain[B]

      def resource[R, O](acquire: IO[R])(use: R => Process[IO, O])(release: R => Process[IO, O]) = await[IO, R, O](acquire) {
        case Left(t) => Halt(t)
        case Right(r) => use(r).onComplete(release(r))
      }

      // Full example showing how to manage source using our new constructs
      def linesExample(fileName: String): Process[IO, String] =
        resource
          {IO{io.Source.fromFile(fileName)}}
      {src =>
        lazy val lines = src.getLines()
        def step = if (lines.hasNext) Some(lines.next) else None
        lazy val linesProcess : Process[IO, String] = eval(IO(step)).flatMap {
          case None => Halt(End)
          case Some(line) => Emit(line, linesProcess)
        }
        linesProcess
      }
      {
        src => eval_{IO{src.close()}}
      }


      // This is supposed to help constrain the Input type of the process
      // If Is[I] is defined, supposed to constrain f[X] to be f[I]
      // This is because the Get is the only way that the f[X] instance can be returned
      case class Is[I]() {
        sealed trait f[X]
        val Get = new f[I] {}
      }

      def Get[I] = Is[I]().Get

      type Process1[I, O] = Process[Is[I]#f, O]

      def halt1[I, O]: Process1[I, O] = Halt[Is[I]#f, O](End)

      def await1[I, O](recv: I => Process1[I, O], fallback: Process1[I, O] = halt1[I, O]) : Process1[I, O] = {
        Await(Get[I], (e: Either[Throwable, I]) => e match {
          case Left(End) => fallback
          case Left(err) => Halt(err)
          case Right(i) => Try(recv(i))
        })
      }

      def emit1[I, O](h: O, t1: Process1[I, O] = halt1[I, O]) : Process1[I, O] = emit(h, t1)

      def lift1[I, O](f: I => O) : Process1[I, O] = await1((i: I) => emit(f(i)).asInstanceOf[Process1[I, O]]) repeat

      def filter1[I](f: I=> Boolean): Process1[I, I] = await1(i => {
        if(f(i)) emit1(i) else halt1 repeat
      })

      // Multiple Input Streams
      case class T[I, I2]() {
        sealed trait f[X] {def get: Either[I => X, I2 => X]}
        val L = new f[I] {def get = Left(identity)}
        val R = new f[I2] {def get = Right(identity)}
      }

      def L[I, I2]() = T[I, I2]().L
      def R[I, I2]() = T[I, I2].R

      type Tee[I, I2, O] = Process[T[I, I2]#f, O]

      def haltT[I, I2, O]: Tee[I, I2, O] = Halt[T[I, I2]#f, O](End)

      def awaitL[I, I2, O](recv: I => Tee[I, I2, O],
                           fallback: => Tee[I, I2, O] = haltT[I, I2, O]) : Tee[I, I2, O] = {
        await[T[I, I2]#f, I, O](L()) {
          case Left(End) => fallback
          case Left(err) => Halt(err)
          case Right(x) => Try(recv(x)).asInstanceOf[Tee[I, I2, O]]
        }
      }

      def awaitR[I, I2, O](receive: I2 => Tee[I, I2, O],
                           fallback: => Tee[I, I2, O] = haltT[I, I2, O]) : Tee[I, I2, O] = {
        await[T[I, I2]#f, I2, O](R()) {
          case Left(End) => fallback
          case Left(err) => Halt(err)
          case Right(a) => Try(receive(a))
        }
      }

      def emitT[I, I2, O](h: O, t: Tee[I, I2, O] = haltT) : Tee[I, I2, O] = emit(h, t)

      def zipWith[I, I2, O](f: (I, I2) => O) : Tee[I, I2, O] = awaitL[I, I2, O]((i: I) => awaitR ((i2 : I2) => emitT(f(i, i2), haltT[I, I2, O]))).repeat

      def zip[I, I2] : Tee[I, I2, (I, I2)] = zipWith((_, _))

      type Sink[F[_], O] = Process[F, O => Process[F, Unit]]

      def constant[A](a: A) : Process[IO, A] = eval[IO, A](IO(a)).repeat

      def fileW(file: String, append : Boolean = false) : Sink[IO, String] = {
        resource[FileWriter, String => Process[IO, Unit]]
        {
          IO { new FileWriter(file, append)}
        }
        {
          writer => constant {(s: String) => eval[IO, Unit](IO(writer.write(s)))}
        }
        {
         writer => eval_(IO(writer.close()))
        }
      }

      // Ex 15.12
      def join[F[_], O](p: Process[F, Process[F, O]]) : Process[F, O] = p.flatMap(pa => pa)

    }
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

    val numberStream = Stream(1, 2.5, 3.5, 6, 2, 100, 7)
    // takeWhile
    val takeWhileResult = takeWhile((x: Double) => x < 4)(numberStream)
    println(takeWhileResult.toList)

    // dropWhile
    val dropWhileResult = dropWhile((x: Double) => x < 4)(numberStream)
    println(dropWhileResult.toList)

    // count
    val countResult = count(wordStream)
    println(countResult.toList)

    // mean
    val meanResult = mean(numberStream)
    println(meanResult.toList)

    // sumLoop
    val sumLoopResult = sumLoop(numberStream)
    println(sumLoopResult.toList)

    // countLoop
    val countLoopResult = countLoop(numberStream)
    println(countLoopResult.toList)

    //zipWithIndex
    val zipResult = count.zipWithIndex(wordStream)
    println(zipResult.toList)

    // Generic zip
    val zipMeanResult = sum.combineWithOperation(count, (currentSum: Double, currentCount: Int) => currentSum / currentCount)(numberStream)
    println(zipMeanResult.toList)

    // exists method
    val existsResult = exists((x: Double) => x % 2 == 0)(numberStream)
    println(existsResult.toList)

    // file processor
    // count number of lines and determine if more than 40000

    def moreThan40000 = processFile(count |> exists(_ > 40000), false)(_ || _)_
    val fileSource = new File("/Users/benjaminettori/Projects/FootballStatsManager/fmstats-ui/webpack.config.js")
    val fileResult = moreThan40000(fileSource)
    println(fileResult)

    // file processor
    // convert temp to celsius
    val fahrFile = new File("/Users/benjaminettori/Documents/Fahrenheit.txt")
    val convertResult = getCelsius(fahrFile)
    println(convertResult)

  }


}
