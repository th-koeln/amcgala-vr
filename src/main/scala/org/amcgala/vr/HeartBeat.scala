package org.amcgala.vr

import akka.actor.{ Actor, Props }

object HeartBeat {

  case class Tick(time: Time)
  case object Ping

  def props(): Props = Props(new HeartBeat())

}

/**
  * Publishes the [[HeartBeat.Tick]] messages to the [[akka.event.EventStream]] of the [[akka.actor.ActorSystem]].
  */
class HeartBeat extends Actor {

  import concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import HeartBeat._
  var hours = 0
  var minutes = 0

  context.system.scheduler.schedule(2.second, 100.millis, self, Ping)

  def receive: Actor.Receive = {
    case Ping ⇒
      minutes = (minutes + 5) % 60
      hours = if (minutes == 0) (hours + 1) % 24 else hours
      println(s"$hours::$minutes")
      context.system.eventStream.publish(Tick(Time(hours, minutes)))
  }
}

case class Time(hours: Int, minutes: Int)