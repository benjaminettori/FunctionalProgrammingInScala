import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import akka.actor.Actor.Receive
import akka.actor._
import akka.pattern.ask

/**
  * Created by bettori on 9/30/2017.
  */
object Chapter7 {

  val system = ActorSystem("test")

  def sum(ints: Seq[Int]): Int = {
    ints.foldLeft(0)((a, b) => a + b)
  }

  def sum_par_initial(ints: Seq[Int]): Int = {
    if(ints.size <= 1){
      ints.headOption.getOrElse(0)
    } else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }
  }

  type Par[A] = ExecutorService => Future[A]

  object Par {
    def unit[A](a: A) : Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      override def isCancelled = false

      override def get(timeout: Long, unit: TimeUnit): A = get

      override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

      override def isDone: Boolean = true
    }

    // Ex 7.3
    case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
      @volatile var cache: Option[C] = None // warning this variable may be changed by other threads. Forces JVM to update variable when changes occur in other threads
      def isDone = cache.isDefined
      def isCancelled = a.isCancelled || b.isCancelled
      def cancel(evenIfRunning: Boolean) = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
      def get = compute(Long.MaxValue)
      def get(timeout: Long, units: TimeUnit): C = compute(TimeUnit.MILLISECONDS.convert(timeout, units))

      private def compute(timeoutMs: Long): C = cache match {
        case Some(c) => c
        case None => {
          val start = System.currentTimeMillis
          val ar = a.get(timeoutMs, TimeUnit.MILLISECONDS)
          val stop = System.currentTimeMillis
          val at = stop - start
          val br = b.get(timeoutMs - at, TimeUnit.MILLISECONDS)
          val ret = f(ar, br)
          cache = Some(ret)
          ret
        }
      }
    }

    // Ex 7.1
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
      val (af, bf) = (a(es), b(es))
      Map2Future(af, bf, f)
    }

    // ExecutorService submit submits a task for execution to the executor service and returns a future.
    def fork[A](a: => Par[A]) : Par[A] = es => {
      es.submit(new Callable[A] {
         def call = a(es).get
      })
    }
    def lazyUnit[A](a: => A) : Par[A] = fork(unit(a))
    def run[A](s: ExecutorService)(a: Par[A]) : Future[A] = a(s)

    // Ex 7.4
    def asyncF[A, B](f: A => B): A => Par[B] = a => {
      lazyUnit(f(a))
    }

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
      map2(pa, unit(()))((a, _) => f(a))
    }

    // Ex 7.5
    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
     ps.foldRight(unit(List[A]()))((a, p) => map2(a, p)((a, b) => a :: b))
    }

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

    //Ex 7.6
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars: List[Par[List[A]]] = as map (asyncF((a: A) => if(f(a)) List(a) else List())) // Need to use lists here because if use type A, what would be returned in else clause?
      map(sequence(pars))(a => a.flatten)
    }

    def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = {
      map2(p, p2)(_ == _)
    }
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if(ints.size <= 1) {
      Par.unit(ints.headOption.getOrElse(0))
    } else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
  }

  def countWords(paragraphs: List[String]): Par[Int] = {
    paragraphs match {
      case h :: t => Par.map2(Par.map2(Par.unit(h.split(" ").length), Par.unit(t.head.split(" ").length))(_ + _), countWords(t.tail))(_ + _)
      case h :: Nil => Par.unit(h.split(" ").length)
      case Nil => Par.unit(0)
    }
  }

  // Problem: the current implementation requires the current thread to block on get method of Future\
  // Instead we register a callback to avoid blocking on the get method
  sealed trait CustomFuture[A] {
    def apply(k: A => Unit): Unit
  }

  type CustomPar[A] = ExecutorService => CustomFuture[A]

  // This method will run a parallel computation of type CustomPar and return the result
  // can give it different methods that produce par values.
  // Separation of concerns
  def run[A](es: ExecutorService)(p: Either[Throwable, CustomPar[A]]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    // register callback
    p match {
      case Left(e) => throw e
      case Right(parA) => parA(es) (a => {
        ref.set(a)
        latch.countDown()
      })
    }
    latch.await()
    ref.get()
  }

  def unit[A](a: A): CustomPar[A] = es => new CustomFuture[A] {
    def apply(cb: A => Unit): Unit = cb(a)
  }

  // When the CustomFuture returned by fork receives it's continuation cb
  // it will fork off a task to evaluate the argument a. Once the argument has been evaluated
  // and called to produce a CustomFuture[A], we register cb to be invoked on the resulting A
  def fork[A](a: => CustomPar[A]): CustomPar[A] = es => new CustomFuture[A] {
    def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
  }

  // Create new Callable task that executes the callback r, which is passed as a lazy variable
  // r gets evaluated when call is evaluated.
  // r gets evaluated in another thread when the callable is created.
  def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] {
    def call = r
  })

  class CustomActor (receiveMethod: Receive) extends Actor {
    override def receive: Receive = receiveMethod
  }

  def map2[A, B, C](p: CustomPar[A], p2: CustomPar[B])(f: (A, B) => C): CustomPar[C] = {
    es => new CustomFuture[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val actorReceiver: Receive = {
          case Left(a: A) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)))
          }

          case Right(b: B) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)))
          }
        }

        val combinerProps = Props(classOf[CustomActor], actorReceiver)
        val combiner = system.actorOf(combinerProps)
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }
  }

  def lazyUnit[A](a: => A) : CustomPar[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => CustomPar[B] = a => {
    lazyUnit(f(a))
  }

  def sequence[A](ps: List[CustomPar[A]]): CustomPar[List[A]] = {
    ps.foldRight(unit(List[A]()))((a, p) => map2(a, p)((a, b) => a :: b))
  }

  def parMap[A, B](ps: List[A])(f: A => B): CustomPar[List[B]] = fork {
    val fbs: List[CustomPar[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // Ex 7.11
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es => {
      val test = Par.run(es)(n: Par[Int]).get
      choices(test)(es)
    }
  }

  // Ex 7.12
  def choiceMap[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = {
    es => {
      val k= Par.run(es)(key).get
      val choice = choices(k)
      choice(es)
    }
  }

  // Ex 7.13
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es => {
      val key = Par.run(es)(pa).get
      val choice = choices(key)
      choice(es)
    }
  }

  // Chooser is the primitive function, choiceN is expressed using chooser
  def choiceNchooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    chooser(n)(x => choices(x))
  }

  // Ex 7.14

  def join[A](a: Par[Par[A]]): Par[A] = {
    es => {
      val parA = Par.run(es)(a).get
      Par.run(es)(parA)
    }
  }

  def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] = {
    join(Par.map(p)(f))
  }

  def main(args: Array[String]): Unit = {
    val list = List("A first paragraph", "A second paragraph")
    val execS = Executors.newFixedThreadPool(3)
    println(countWords(list)(execS).get())

    val p = parMap(List.range(1, 1000))(math.sqrt(_))
    val x = run(Executors.newFixedThreadPool(2))(Right(p))
    println(x)
  }
}
