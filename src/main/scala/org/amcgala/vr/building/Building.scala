package org.amcgala.vr.building

import org.amcgala.vr.{Position, SimulationAgent, Agent}
import akka.actor.{Stash, ActorRef}
import org.amcgala.vr.SimulationAgent.PositionChange
import org.amcgala.vr.BotAgent.Introduction


object Building {

  case class RegisterOwner(owner: ActorRef)

}

trait Building extends Agent with Stash {

  import Building._

  var owner: Option[ActorRef] = None
  var simulation: ActorRef = ActorRef.noSender
  var localPosition: Position = Position(0, 0)

  override def postStop(): Unit = {
    simulation ! SimulationAgent.Unregister
  }

  def receive: Receive = {
    case Introduction ⇒
      simulation = sender()
    case PositionChange(pos) ⇒
      localPosition = pos
      context.become(common)
      unstashAll()
    case _ ⇒ stash()
  }

  def common: Receive = {
    case RegisterOwner(o) => owner = Some(o)
  }

}

sealed trait BuildingType

object BuildingType {

  case object Restaurant extends BuildingType

}
