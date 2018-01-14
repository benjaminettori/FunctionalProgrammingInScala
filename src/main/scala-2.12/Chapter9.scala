import Chapter8._
import scala.util.matching.Regex
object Chapter9 {

  // Parsers[+_] : This _ indicates that Parser is a type constructor. + is the regular covariant symbol
  // Thus we can write p: Parser[A] to complete type construction in run() (or Parser[Char] in char())

  // self => : This syntax assigns instance of Parsers to the variable self

  trait Parsers { self =>

    type Parser[A] = Location => Result[A]

    trait Result[+A] {
      def mapError(f: ParseError => ParseError) : Result[A] = this match {
        case Failure(e, c) => Failure(f(e), c)
        case _ => this
      }

      def uncommit(): Result[A] = {
        this match {
          case Failure(g, true) => Failure(g, false)
          case _ => this
        }
      }

      def addCommit(commit: Boolean): Result[A] = {
        this match {
          case Failure(e, c) => Failure(e, c || commit)
          case _ => this
        }
      }

      def advanceSuccess(n: Int): Result[A] = {
        this match {
          case Success(g, c)=> Success(g, c + n)
          case _ => this
        }
      }
    }

    case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
    case class Failure(get: ParseError, committed: Boolean) extends Result[Nothing]

    // stack is the list of error messages indicating what Parser was doing when it failed.
    // if run(p)(s) is Left(e1), then run(scope(msg)(p)) is Left(e2) where e2.stack.Head is msg, e2.stack.tail is e1
    case class ParseError(stack: List[(Location, String)] = List(), otherErrors: List[ParseError] = List()) {
      // copy is a method to clone the existing class and update the stack variable.
      def push(loc: Location, msg: String) : ParseError = copy(stack = (loc, msg) :: stack)

      def latest: Option[(Location, String)] = stack.lastOption
      def latestLoc: Option[Location] = latest.map(v => v._1)

      // replace contents of stack with last location and new message
      def label[A](s: String) : ParseError = ParseError(latestLoc.map(v => (v, s)).toList)

      override def toString =
        if (stack.isEmpty) "no error message"
        else {
          val collapsed = collapseStack(stack)
          val context =
            collapsed.lastOption.map("\n\n" + _._1.line).getOrElse("") +
              collapsed.lastOption.map("\n" + _._1.col).getOrElse("")
          collapsed.map { case (loc,msg) => loc.line.toString + "." + loc.col + " " + msg }.mkString("\n") +
            context
        }

      def addError(error: ParseError): ParseError = this.copy(stack, error :: this.otherErrors)

      def collapseStack(s: List[(Location,String)]): List[(Location,String)] =
        s.groupBy(_._1).
          mapValues(_.map(_._2).mkString("; ")).
          toList.sortBy(_._1.offset)

      def formatLoc(l: Location): String = l.line + "." + l.col
    }


    def run[A](p: Parser[A])(input: String): Either[ParseError, A]
    def char(c: Char): Parser[Char] = scope(s"Currently processing character $c")(loc => {
      if(loc.input.slice(loc.offset, loc.input.length).startsWith(c.toString)) {
        Success(c, loc.offset + 1)
      } else {
        Failure(ParseError(List((loc, "Could not find character in current string"))), committed = true)
      }
    })

    // 2nd parameter is non-strict as we may not need to evaluate it.
    def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

    // this enables us to write : "abracadabra" to get parser of type string directly, instead of having to call string()
    implicit def string(s: String): Parser[String] = scope(s"currently parsing string $s")(loc => {
      if (loc.input.slice(loc.offset, loc.input.length).startsWith(s)) {
        Success(s, loc.offset + s.length)
      } else {
        Failure(ParseError(List((loc, "Could not parse current string"))), true)
      }
    })

    // this converts Parser to ParserOps, giving Parser access to ParserOps methods
    implicit def operators[A](p: Parser[A]) : ParserOps[A] = ParserOps[A](p)

    // Take any value and convert it to Parser[String] as long as implicit conversion function f is defined somewhere
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    // convert regex to Parser[String]
    implicit def regex(r: Regex): Parser[String]

    // define a method where Parser p is repeated n times
    // useful if want to define test where we wish to find n reps of the same pattern
    // Ex 9.4: implementation
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
      if( n <= 0) succeed(List[A]())
      else map2(p, listOfN(n - 1, p))((a, b) => a :: b)
    }

    // Define operations on Parser
    // combined with implicit conversions defined above we can write:
    // "ab" || "bca" directly and get a Parser[String] which we can run

    // Ex: run("bca" || "abc")("bcarty") will work and return Right("bcarty") (since it finds the string "bca" in the input string)
    case class ParserOps[A](p: Parser[A]) {
      def ||[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

      // With the 2 following operations we can write:
      // val numA: Parser[Int] = char('a').many.map(_.size) (this takes a Parser[Char] and transforms it to a Parser[Int], where the Int is the number of occurrences of 'a'
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
      def many(): Parser[List[A]] = self.many(p)

      // With this method we can write run(char('a').slice.map(_.size))("aaa") and it should return Right("aaa")
      def slice(): Parser[String] = self.slice(p)

      // Use this to call char('a') ** char('b') in order to run char('a') and then char('b') and return a combined result.
      def **[B](pb: Parser[B]): Parser[(A, B)] = self.product(p, pb)
    }

    // ignore left side of input
    // this is better than extractL because we don't need to evaluate both parsers.
    def skipL[B](p1: Parser[Any], p2: => Parser[B]) = map2(p1, p2)((_, b) => b)

    def skipR[A](p1: Parser[A], p2: => Parser[Any]) = map2(p1, p2)((a, _) => a)

    def extractL[B](p: Parser[(Any, B)]): Parser[B] = map(p)(a => a._2)

    // This is an abstraction of the operation that takes a Char and returns a list of the occurrences of that Char in a String.
    // Ex 9.3: implementation of many.
    // Recursive implementation. In theory, many(p) can evaluate to List[A]() if map2 evaluation fails.
    def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))((a, b) => a::b) || succeed(List[A]())


    // Ex 9.5
    // wrap parser in non-strict variable to avoid evaluating it if it's not needed
    def wrap[A](p: => Parser[A]) : Parser[A]

    def manyWrap[A](p: Parser[A]): Parser[List[A]] = map2(p, wrap(many(p)))(_ :: _) || succeed(List[A]())

    // The map operation for Parser
    // Ex 9.8 -> implementation
    def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(a => succeed(f(a)))

    // This combinator succeeds regardless of the input string
    // run(succeed(a))("test") returns Right(a)
    // succeed allows us to generate a Parser[A] from a value of A.
    def succeed[A](a: A): Parser[A] = string("").map(_ => a)

    // Returns the portion of the string examined by the parser if the parser is successful.
    def slice[A](p: Parser[A]): Parser[String]

    // runs a one parser and then another and returns the combined result
    // 2nd parameter is non-strict because first parameter might fail, in which case there is no need to evaluated 2nd param.
    // Ex 9.7 -> implementation
    def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] = flatMap(pa)(a => map(pb)(b =>(a,b)))

    // Ex 9.1
    // Combine two parsers into a third.
    // 2nd argument must be non-strict to ensure it is only evaluated if needed.
    // Ex 9.7 -> implement with flatMap
    def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] = for {a <- pa; b <- pb} yield f(a, b) // can use for comprehension because have definition for map and flatMap.
    //product(pa, pb).map(c => f(c._1, c._2))

    // Ex 9.1
    // Require 1 or more char values in a a string
    def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many())((a, b) => a :: b)

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    // Ex 9.6
    // Example of writing parser to parse expressions like "4aaaa" or "2aa", where number of letters must match number.
    def contextParser(pb: Parser[Char]): Parser[List[Char]] = {
      val r = "[0-9]+".r // regex for digits. .r transforms string to regex

      // implicit conversion of Regex to Parser[String] allows us to use flatmap with Regex.
      flatMap(r)(a => listOfN(a.toInt, pb))
    }

    /**
      * These methods provide regex based parsers for certain kinds of inputs
      * */
    def string: Parser[String] =  "\"(.)*\"".r

    def digit : Parser[String] = "[0-9]+".r

    def doubleString : Parser[String] = "[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r

    def double : Parser[Double] = doubleString.map(d => d.toDouble)

    /**
      * Define laws that parsers must obey
      * */
    // use this object to define the laws that the parser combinator must hold.
    object Laws {

      /** Helper methods */
      // use this method to test equality between Parsers
      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = {
        Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))
      }

      // Ex 9.2
      def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)
      def unbiasR[A, B, C](p:  (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

      /** Laws */

      // Define law that states that map(p)(a => a) == p
      // ie map should be structure preserving.
      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = {
        equal(p.map(a => a), p)(in)
      }

      def succeedLaw[A](a: A)(in: Gen[String]): Prop = {
        Prop.forAll(in)(s => run(succeed(a))(s) == Right(a))
      }

      // Ex 9.2
      def productAssociativeLaw[A, B, C](pa: Parser[A], pb: Parser[B], pc: Parser[C])(in: Gen[String]): Prop = {
        equal(((pa ** pb) ** pc).map(r => unbiasL(r)), (pa ** (pb ** pc)).map(r => unbiasR(r)))(in)
      }

      // Ex 9.2
      def productMapLaw[A, B, C](pa: Parser[A], pb: Parser[B], f: A => C, g: B => C)(in: Gen[String]): Prop = {
        equal((pa ** pb).map { case (a, b) => (f(a), g(b))}, pa.map(a => f(a)) ** pb.map(b => g(b)))(in)
      }
    }

    /** Error Reporting */
    // Ex 9.10
    // If p fails, it's ParserError will display msg.
    def label[A](msg: String)(p: Parser[A]) : Parser[A] = loc => p(loc).mapError(err => err.label(msg))

    // Method to answer question: Where did the error occur?
    // offset value would presumably be returned from ParseError object
    case class Location(input: String, offset: Int = 0) {
      lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
      lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
        case -1 => offset + 1
        case lineStart => offset - lineStart
      }

      // Move location forward by n characters.
      // copy current location by n
      def advanceBy(n: Int): Location = copy(offset = offset + n)
    }

    // Extract error location and error message
    def errorLocation(e: ParseError) : Location
    def errorMessage(e: ParseError) : String

    // Provide scope on what Parser was doing when it failed
    // Msg provides that information
    def scope[A](msg: String)(p: Parser[A]) : Parser[A] = {
      loc => p(loc).mapError(err => err.push(loc, msg))
    }

    // Attempt a parser p, and if it fails, report parser error, unless p failed immediately, in which case try p2.
    // Example: (attempt("abra" ** "abra") ** "cadabra") || ("abra" ** "cadabra")
    // Here the parser will attempt to parse 2 abra values in a row. If first "abra" is parsed and then we get error, it will
    // revert to second branch of or statement (abra cadabra)
    // If 2nd abra is parsed, parser commits to parsing first branch of or, and if error is encountered it will not parse
    // 2nd branch.
    def attempt[A](p: Parser[A]): Parser[A]

    // return error that occurred after consuming largest number of characters.
    def furthest[A](p: Parser[A]) : Parser[A]

    // return error that occurred most recently
    def latest[A](p: Parser[A]) : Parser[A]
  }

  trait JSON
  object JSON {
    case object JNull extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON

    // Ex 9.9
    // Design a JSON parser
    def jsonParser[Err](P: Parsers): P.Parser[JSON] = {
      import P._
      val openingBrace = char('{')
      val comma = char(',')
      val closingBrace = char('}')
      val openingBracket = char('[')
      val closingBracket = char(']')
      val stringRegex = string
      val keyParser = skipR(stringRegex, char(' ') ** char(':')) //(stringRegex ** char(' ') ** char(':')).map(a => Laws.unbiasL(a)._1) // capture "key" : and return only the "key"

      // need to use def instead of val because we are using a forward reference for entry and keyVal
      def jNullParser = string("null").map(_ => JNull)
      def JStringParser = stringRegex.map(s => JString(s))
      def boolParser = (string("true") || string("false")).map(b => JBool(  b.toBoolean))
      def doubleParser = double.map(d => JNumber(d))
      def objectLiteralParser = jNullParser || JStringParser || boolParser || doubleParser
      def jArrayParser = (openingBracket ** (entry ** comma).many.map(c => JArray(c.map(_._1).toIndexedSeq))  ** closingBracket).map(c => Laws.unbiasL(c)._2)
      def jObjectParser = (openingBrace ** (keyVal ** comma).many.map(c => JObject(Map(c.map(_._1) : _*))) ** closingBrace).map(c => Laws.unbiasL(c)._2)
      def entry: Parser[JSON] = objectLiteralParser || jObjectParser || jArrayParser
      def keyVal = keyParser ** entry

      // take a list of key-value pairs, map them to one Map, and create JObject from it
      jObjectParser
    }
  }



  def main(args: Array[String]): Unit = {
    object myParsers extends Parsers {
      // Ex 9.15
      override def run[A](p: myParsers.Parser[A])(input: String): Either[ParseError, A] = {
        val location = Location(input)
        p(location) match {
          case Success(r, _) => Right(r)
          case Failure(pe, _) => Left(pe)
        }
      }

      override def or[A](s1: myParsers.Parser[A], s2: => myParsers.Parser[A]): myParsers.Parser[A] = loc => {
        s1(loc) match {
          case Failure(prevErr, false) => s2(loc).mapError(pe => pe.addError(prevErr))
          case x => x
        }
      }


      // Ex 9.13
      override implicit def regex(r: Regex): myParsers.Parser[String] = loc => {
        r.findPrefixOf(loc.input) match {
          case Some(s) => Success(s, loc.offset + s.length)
          case None => Failure(ParseError(List((loc, "Could not match regex."))), true)
        }
      }

      override def wrap[A](p: => myParsers.Parser[A]): myParsers.Parser[A] = p

      // Retrieve portion of string that was processed by parser
      override def slice[A](p: myParsers.Parser[A]): myParsers.Parser[String] = loc => {
        p(loc) match {
          case Success(_, n) => Success(loc.input.slice(loc.offset, loc.offset + n), n)
          case f@Failure(_, _) => f
        }
      }

      /**
        * If the first parser is a success, execute the second parser after advancing the location by n
        * Commit the result if at least 1 character was consumed
        * On success of the second cursor, account for result of first cursor by incrementing characters consumed by n
        *  */
      override def flatMap[A, B](p: myParsers.Parser[A])(f: (A) => myParsers.Parser[B]): myParsers.Parser[B] = loc => {
        p(loc) match {
          case Success(g, n) => f(g)(loc.advanceBy(n)).addCommit(n > 0).advanceSuccess(n)
          case e@Failure(_, _) => e
        }
      }

      /** Error Reporting */

      override def errorLocation(e: myParsers.ParseError): myParsers.Location = ???

      override def errorMessage(e: myParsers.ParseError): String = ???

      override def attempt[A](p: myParsers.Parser[A]): myParsers.Parser[A] = loc => p(loc).uncommit()

      override def furthest[A](p: myParsers.Parser[A]): myParsers.Parser[A] = ???

      override def latest[A](p: myParsers.Parser[A]): myParsers.Parser[A] = ???
    }

    val test = "Hello and welcome"
    val testLocation = new myParsers.Location(test)
    val subString = "ello and"
    val result = myParsers.string(subString)
    println(result(testLocation))
  }
}
