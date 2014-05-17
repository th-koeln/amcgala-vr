package org.amcgala.vr

import akka.actor.{ Actor, Props }

object HeartBeat {

  case class Tick(time: Time)

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

  context.system.scheduler.schedule(2.second, 50.millis, self, Tick(Time(hours, minutes)))

  def receive: Actor.Receive = {
    case t: Tick ⇒
      minutes = (minutes + 1) % 60
      hours = if(minutes == 0) (hours + 1) % 24 else hours + 1

      context.system.eventStream.publish(t)
  }
}

case class Time(hours: Int, minutes: Int)