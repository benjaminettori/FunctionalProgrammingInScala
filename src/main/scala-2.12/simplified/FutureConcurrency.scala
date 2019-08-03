package simplified
import scala.concurrent.Future
import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global


object FutureConcurrency {

  def sleep(time: Long): Unit = Thread.sleep(time)
  def currentTime = System.currentTimeMillis()
  def deltaTime(t0: Long) = currentTime - t0

  val f1 = Future {sleep(100); 1}
  val f2 = Future { sleep(150); 1}
  val f3 = Future { sleep(200); 3}

  val startTime = currentTime
  var fs1ThreadId = 0L
  var fs2ThreadId = 0L
  var fs3ThreadId = 0L

  val fs1 = Future[Int] {
    println(s"fs1 start: ${deltaTime(startTime)}")
    fs1ThreadId = Thread.currentThread().getId
    sleep(1200)
    1
  }

  val fs2 = Future[Int] {
    println(s"fs2 start: ${deltaTime(startTime)}")
    fs2ThreadId = Thread.currentThread().getId
    sleep(1200)
    2
  }

  val fs3 = Future[Int] {
    println(s"fs3 start: ${deltaTime(startTime)}")
    fs3ThreadId = Thread.currentThread().getId
    sleep(1200)
    3
  }


  def main(args: Array[String]) = {
    val result = for {
      r1 <- f1
      r2 <- f2
      r3 <- f3
    } yield  r1 + r2 + r3

    result.onComplete {
      case Success(s) => println(s"result is $s")
      case scala.util.Failure(exception) => println(s"Failure because ${exception.getMessage}")
    }

    sleep(300)


    val mainThreadId = Thread.currentThread().getId

    println(s"before for comp: ${deltaTime(startTime)}")

    val resultf = for {
      fr1 <- fs1
      fr2 <- fs2
      fr3 <- fs3
    } yield (deltaTime(startTime), fr1 + fr2 + fr3)

    println(s"after for comp: ${deltaTime(startTime)}")

    println(s"before onComplete: ${deltaTime(startTime)}")

    resultf.onComplete {
      case Success(value) => {
        sleep(10)
        val tInSuccessCase = deltaTime(startTime)
        println(s"in success case: ${tInSuccessCase}")
        println(s"result: $value")
        println(s"onComplete tid: ${Thread.currentThread().getId}")
      }
      case scala.util.Failure(exception) => exception.printStackTrace()
    }

    println(s"after onComplete: ${deltaTime(startTime)}")

    println(s"start sleep(2000): ${deltaTime(startTime)}")
    sleep(2000)

    println("")
    println("Thread IDs")
    println("------------")
    println(s"Main Thread ID: $mainThreadId")
    println(s"fs1 Thread ID: ${fs1ThreadId}")
    println(s"fs2 Thread ID: ${fs2ThreadId}")
    println(s"fs3 Thread ID: ${fs3ThreadId}")

  }


}
