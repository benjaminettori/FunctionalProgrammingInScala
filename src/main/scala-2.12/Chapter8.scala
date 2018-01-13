import java.util.concurrent.Executors

import Chapter6.{RNG, ScalaState, SimpleRNG}
import Chapter7.{Par}
import Chapter8.Prop._

/**
  * Created by bettori on 11/22/2017.
  */
object Chapter8 {
  //def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  // Syntactic sugar for defining pairs of objects. Instead of (s, a) have s ** a. Instead of (s, (a, b)), have s ** a ** b
  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  object Prop {
    type MaxSize = Int
    sealed trait Result {
      def isFalsified: Boolean
    }
    final case object Passed extends Result {
      override def isFalsified = false
    }

    final case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      override def isFalsified = true
    }

    final case object Proved extends Result {
      override def isFalsified = true
    }

    type FailedCase = String
    type SuccessCount = Int
    type TestCases = Int

    def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
      (max, n, rng) => randomStream(gen)(rng).zip(Chapter5.Stream.from(0)).take(n).map {
          case (a, i) => try {
            if (f(a)) Passed else Falsified(a.toString, i)
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i)
          }
      }.find(p => p.asInstanceOf[Result].isFalsified).getOrElse(Passed).asInstanceOf[Result] // Product with Serializable issue requires explicit cast
        // https://underscore.io/blog/posts/2015/06/04/more-on-sealed.html
    }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (maxRuns, n, rng) => {
        val casesPerSize = (n -1) / maxRuns + 1
        // generate several props (up to max) each with a list of test cases of different sizes. The test case size is determined by casesPerSize.
        // max indicates how many times to run the test (at a maximum), and test cases per size is determined from max and the number n.
        // Therefore, the total number of test cases is divided among the different potential sizes.
        // Max size is the max number of different test runs we want.
        val props: Chapter5.Stream[Prop] = Chapter5.Stream.from(0).take((n min maxRuns) + 1).map(i => forAll(g(i))(f)).asInstanceOf[Chapter5.Stream[Prop]]
        val prop: Prop = props.map(p => Prop {
          (max, _, rng) => p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
        prop.run(maxRuns, n, rng)
      }
    }
    def randomStream[A](g: Gen[A])(rng: RNG): Chapter5.Stream[A] = Chapter5.Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
    def buildMsg[A](s: A, e: Exception): String = s"test case: $s\n" + s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    // This is a weighted combination of fixed and unbounded thread pools
    val S = Gen.weighted(
        Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
        Gen.unit(Executors.newCachedThreadPool) -> 0.25
      )

    // syntactic sugar for evaluating parallel library properties
    // want to mask complexity of handling Par objects.
    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = {
      val execCombo = S ** g
      forAll(execCombo){ case s ** a => f(a)(s).get} // equivalent to p => p match {case (s, a) ....
    }

    def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = SimpleRNG(System.currentTimeMillis())): Unit = {
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
        case Passed => println(s"+ OK, passed $testCases tests")
        case Proved => println(s"+ OK, proved property")
      }
    }

    def check(p: => Boolean): Prop = Prop {(_, _, _) => { if (p) Proved else Falsified("()", 0) }}
    def checkPar(p: => Par[Boolean]): Prop = forAllPar(Gen.unit())(_ => p)
  }

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    // Ex 8.3
    //    def &&(prop: Prop): Prop = new Prop {
    //      def check = Prop.this.check && prop.check
    //    }

    // Ex 8.9
    def &&(prop: Prop): Prop = Prop {
      (max, n, rng) => {
        run(max, n, rng) match {
          case Passed => prop.run(max, n, rng)
          case Proved => prop.run(max, n, rng)
          case x => x
        }
      }
    }

    def ||(prop: Prop): Prop = Prop {
      (max, n, rng) => {
        run(max, n, rng) match {
          case Passed => Passed
          case Proved => Proved
          case Falsified(m, _) => prop.tag(m).run(max, n, rng)
        }
      }
    }

    def tag(msg: String) = Prop {
      (max, n, msg) => {
        run(max, n, msg) match {
          case Falsified(m, p) => Falsified(msg + "\n" + m, p)
          case x => x
        }
      }
    }

  }

  object Gen {
    // Ex 8.4
    def choose(start: Int, stopExclusive: Int) : Gen[Int] = {
      Gen(ScalaState(Chapter6.map(Chapter6.nonNegativeInt)(n => start + n % (stopExclusive - start))))
    }

    def choosePair(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
      Gen(ScalaState(outer => {
        val firstGen = choose(start, stopExclusive)
        val first = firstGen.sample.run(outer)
        val secondGen = choose(start, stopExclusive)
        val second = secondGen.sample.run(first._2)
        ((first._1, second._1), second._2)
      }))
    }

    // Ex 8.5
    def unit[A](a: => A): Gen[A] = Gen(ScalaState(rng => (a, rng)))

    def boolean: Gen[Boolean] = Gen(ScalaState(Chapter6.map(Chapter6.nonNegativeInt)(n => n % 2 == 0)))

    // Ex 8.19
    type BoolMapping = Int => Boolean
    def booleanMethod: Gen[BoolMapping] = {
      val mapping: BoolMapping = n => SimpleRNG(n).nextInt._1 % 2 == 0
      choose(1, 100).map(n => mapping)
    }

    def genMethod[A](g: Gen[A]): Gen[Int => A] = {
      Gen(ScalaState(rng => {
        val f = (n: Int) => g.sample.run(SimpleRNG(n))._1
        (f, rng)
      }) )
    }

    def buildListGen[A](n: Int, rng: RNG, gen: Gen[A],  acc: List[A]): List[A] = {
      if (n > 0) {
        val current = gen.sample.run(rng)
        buildListGen(n-1, current._2, gen, current._1 :: acc)
      } else {
        gen.sample.run(rng)._1 :: acc
      }
    }

    def listOfNOld[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(ScalaState(rng => (buildListGen(n, rng, g, List(): List[A]), rng)))

    def getValue[A](option: Gen[Option[A]]): Gen[A] = {
      Gen(ScalaState(rng => {
        val sampleRun = option.sample.run(rng)
       sampleRun._1 match {
          case Some(a) => (a, rng)
          case None => class dd {
            var d: A = _ // default value for type A, only works for class fields
          }
            ((new dd).d, sampleRun._2)
        }
      }))
    }

    // To generate string from Gen[Int], could define a big string with all acceptable alpha-numeric chars.
    // Then use Gen[Int] to generate a random int in the range of the alpha-numeric string to pick a char in it at random.
    // iterate this process up to the length of the string

    // Ex 8.7
    // pull values from either g1 or g2 with equal likelihood
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
      val boolGen = boolean
      boolGen.flatMap(b => if (b) g1 else g2)
    }

    // Ex 8.8
    // Generate rand double and flatmap it using threshold (minProb) to decide which generator to use.
    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)) : Gen[A] = {
      val minProb = g1._2.abs / (g1._2.abs + g2._2.abs)
      Gen(ScalaState(Chapter6.double).flatMap(d => if (d < minProb) g1._1.sample else g2._1.sample))
    }

    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))

    def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n max 1))
  }

  case class Gen[A](sample: ScalaState[RNG, A]) {
    // Ex 8.6
    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      Gen(sample.flatMap(a => f(a).sample))
    }

    def map[B](f: A => B): Gen[B] = {
      Gen(sample.map(f))
    }

    def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = {
      Gen(sample.map2(g.sample)(f))
    }

    // alias for the object method
    def listOfN(size: Int): Gen[List[A]] = {
      Gen.listOfNOld(size, this)
    }

    def listOfN(size: Gen[Int]): Gen[List[A]] = {
      size.flatMap(n => this.listOfN(n))
    }

    // Ex 8.10
    def unsized: SGen[A] = SGen {
     _ => this
    }

    // syntactic sugar for combining 2 Gens into a pair
    def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g)((_,_))
  }

  case class SGen[A](forSize: Int => Gen[A]) {

    // Ex 8.11
    def flatMap[B](f: A => SGen[B]): SGen[B] = SGen {
      n => forSize(n).flatMap(a => {
        f(a).forSize(n)
      })
    }

    def map[B](f: A => B): SGen[B] = SGen {
      n => forSize(n).map(f)
    }

    def map2[B, C](g: SGen[B])(f: (A, B) => C): SGen[C] = SGen {
      n => Gen(forSize(n).sample.map2(g.forSize(n).sample)(f))
    }
  }

  object SGen {
    // Ex 8.12
    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
      n => g.listOfN(n)
    }
  }

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(33)
    val pair = Gen.choosePair(1, 10)
    println(pair.sample.run(rng)._1)
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(Gen.listOf(smallInt))(ns => {
      val max = ns.max
      !ns.exists(_ > max)
    })

    run(maxProp)

    // Ex 8.14
    val listGen = Gen.listOf(smallInt)
    val sortProp = forAll(listGen)(ns => {
      val sorted = ns.sorted
      (ns.isEmpty || sorted.tail.isEmpty || !sorted.zip(sorted.tail).exists(p => p._1 > p._2)) && (!ns.exists(n => !sorted.contains(n))) && !sorted.exists(n => !ns.contains(n))
    })

    run(sortProp)

    val pint = Gen.choose(0, 10) map (Par.unit(_))

    // Ex 8.16
    val pint2 = Gen.choose(-100, 100).listOfN(Gen.choose(0, 10)).map(l => l.foldLeft(Par.unit(0))((p, i) => Par.fork {
      Par.map2(p, Par.unit(i))(_ + _)
    }))

    // Ex 8.17
    // fork(x) == x
    val forkTest = Prop.forAllPar(pint2)(n => Par.equal(n, Par.fork(n)))
    run(forkTest)
  }
}
