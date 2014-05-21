package org.amcgala.vr

import akka.actor.Actor

object Agent {
  case class RegisterOnTickAction(handle: String, action: () ⇒ Unit)
  case class RemoveOnTickAction(handle: String)
}

/**
  * An [[Agent]] represents any active entity in the [[Simulation]]. It depends on an active [[HeartBeat.Tick]] that drives
  * the [[Simulation]] and every active behavior.
  */
trait Agent extends Actor {

  import HeartBeat._
  import Agent._

  /**
    * All registered actions that an [[Agent]] performs for every [[HeartBeat.Tick]]
    */
  private var ticks: Map[String, () ⇒ Unit] = Map()

  private var partials: Vector[Actor.Receive] = Vector()
  private var customReceive: Receive = Actor.emptyBehavior
  private var time = Time(0, 0)

  context.become(receive orElse tickHandling orElse customReceive)
  context.system.eventStream.subscribe(self, classOf[Tick])

  /**
    * Adds the function to the actions that are performed every tick.
    * @param f that function to be added
    */
  def registerOnTickAction(handle: String, f: () ⇒ Unit): Unit = {
    ticks += handle -> f
  }

  def removeOnTickAction(handle: String): Unit = {
    ticks = ticks - handle
  }

  /**
    * [[PartialFunction]] that handles the [[HeartBeat.Tick]] messages
    * @return
    */
  protected def tickHandling: Actor.Receive = {
    case RegisterOnTickAction(handle, action) ⇒
      registerOnTickAction(handle, action)
    case RemoveOnTickAction(handle) ⇒
      removeOnTickAction(handle)
    case t: HeartBeat.Tick ⇒
      time = t.time
      onTick()
  }

  /**
    * Call every function that is registered.
    */
  private def onTick(): Unit = {
    for (f ← ticks) {
      f._2()
    }
  }

  def registerCustomMessage(pf: Actor.Receive): Unit = {
    partials = partials :+ pf
    customReceive = partials.fold(Actor.emptyBehavior)(_ orElse _)
    context.become(receive orElse tickHandling orElse customReceive)
  }

  def currentTime = time

}
