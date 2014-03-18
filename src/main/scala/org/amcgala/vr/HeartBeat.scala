package org.amcgala.vr

import akka.actor.{ Actor, Props }

object HeartBeat {

  case object Tick

  def props(): Props = Props(new HeartBeat())

}

/**
  * Publishes the [[HeartBeat.Tick]] messages to the [[akka.event.EventStream]] of the [[akka.actor.ActorSystem]].
  */
class HeartBeat extends Actor {

  import concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import HeartBeat._

  context.system.scheduler.schedule(1.second, 200.millis, self, Tick)

  def receive: Actor.Receive = {
    case Tick ⇒ context.system.eventStream.publish(Tick)
  }
}