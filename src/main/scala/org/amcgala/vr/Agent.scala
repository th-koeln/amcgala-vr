package org.amcgala.vr

import akka.actor.Actor

/**
  * An [[Agent]] represents any active entity in the [[Simulation]]. It depends on an active [[HeartBeat.Tick]] that drives
  * the [[Simulation]] and every active behavior.
  */
trait Agent extends Actor {

  import HeartBeat._

  context.system.eventStream.subscribe(self, Tick.getClass)

  /**
    * All registered actions that an [[Agent]] performs for every [[HeartBeat.Tick]]
    */
  private var ticks: List[() ⇒ Unit] = List()

  /**
    * Adds the function to the actions that are performed every tick.
    * @param f that function to be added
    */
  def registerOnTickAction(f: () ⇒ Unit): Unit = {
    // TODO this should be a Map (Handler -> Function)
    ticks = f :: ticks
  }

  /**
    * [[PartialFunction]] that handles the [[HeartBeat.Tick]] messages
    * @return
    */
  protected def tickHandling: Actor.Receive = {
    case HeartBeat.Tick ⇒
      onTick()
      customBehaviour()
  }

  /**
    * Call every function that is registered.
    */
  private def onTick(): Unit = {
    for (f ← ticks) {
      f()
    }
  }

  /**
    * Convenience function for a custom behavior that is executed on every [[HeartBeat.Tick]]
    */
  def customBehaviour(): Unit = {}
}
