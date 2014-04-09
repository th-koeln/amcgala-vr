package org.amcgala.vr

import akka.actor.Actor

/**
  * An [[Agent]] represents any active entity in the [[Simulation]]. It depends on an active [[HeartBeat.Tick]] that drives
  * the [[Simulation]] and every active behavior.
  */
trait Agent extends Actor {

  import HeartBeat._

  /**
    * All registered actions that an [[Agent]] performs for every [[HeartBeat.Tick]]
    */
  private var ticks: Vector[() ⇒ Unit] = Vector()

  private var partials: Vector[Actor.Receive] = Vector()
  private var customReceive: Receive = Actor.emptyBehavior

  context.become(receive orElse tickHandling orElse customReceive)
  context.system.eventStream.subscribe(self, Tick.getClass)

  /**
    * Adds the function to the actions that are performed every tick.
    * @param f that function to be added
    */
  def registerOnTickAction(f: () ⇒ Unit): Unit = {
    // TODO this should be a Map (Handler -> Function)
    ticks = ticks :+ f
  }

  /**
    * [[PartialFunction]] that handles the [[HeartBeat.Tick]] messages
    * @return
    */
  protected def tickHandling: Actor.Receive = {
    case HeartBeat.Tick ⇒
      onTick()
  }

  /**
    * Call every function that is registered.
    */
  private def onTick(): Unit = {
    for (f ← ticks) {
      f()
    }
  }

  def registerCustomMessage(pf: Actor.Receive): Unit = {
    partials = partials :+ pf
    customReceive = partials.fold(Actor.emptyBehavior)(_ orElse _)
    context.become(receive orElse tickHandling orElse customReceive)
  }

}
