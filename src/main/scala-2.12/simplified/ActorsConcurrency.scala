package simplified

import akka.actor.{Actor, ActorSystem, Props}

import scala.io.StdIn

object ActorsConcurrency {
  case class Hello(s: String)

  class HelloActor extends Actor {
    override def receive: Receive = {
      case Hello(s) => {
        println(s"You said $s")
        println(s"$s right back at you.")
      }
      case _ => println("huh?")
    }
  }

  case class Message(s: String)
  case object Bye

  class EchoActor extends Actor {
    override def receive: Receive = {
      case Message(s) => println(s"You said $s")
      case Bye => println("See you later")
      case _ => println("huh?")
    }
  }

  def main(args: Array[String]) = {
    val system = ActorSystem("uniqueActorSystem")

    val helloActor = system.actorOf(Props[HelloActor], name = "helloActor")

    helloActor ! Hello("Bonjour")
    helloActor ! Hello("Ni hao")
    helloActor ! "testing"
    val term = system.terminate()

    val echoSystem = ActorSystem("echoUnique")
    val echoActor = echoSystem.actorOf(Props[EchoActor], "echoActor")

    var input = ""
    while (!input.equalsIgnoreCase("q")) {
      println("Type something (q to quit): ")
      input = StdIn.readLine
      if(!input.equalsIgnoreCase("q")) {
        echoActor ! Message(input)
      }
    }

    echoActor ! Bye
    val echoTerm = echoSystem.terminate()
  }
}
