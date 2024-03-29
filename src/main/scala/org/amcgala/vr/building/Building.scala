package org.amcgala.vr.building

import org.amcgala.vr.HeartBeat.Tick
import org.amcgala.vr.{ Coordinate, SimulationAgent, Agent }
import akka.actor.{ Stash, ActorRef }
import org.amcgala.vr.SimulationAgent.PositionChangeRequest
import org.amcgala.vr.BotAgent.Introduction

object Building {

  case class RegisterOwner(owner: ActorRef)
  case class ChangeOrientation(orientation: Double)

}

trait Building extends Agent with Stash {

  import Building._

  var orientation: Double = 0.0

  var owner: Option[ActorRef] = None
  var simulation: ActorRef = ActorRef.noSender
  var localPosition = Coordinate(0, 0)

  override def postStop(): Unit = {
    simulation ! SimulationAgent.UnregisterRequest
  }

  def receive: Receive = {
    case Introduction(townhall) ⇒
      simulation = sender()
    case PositionChangeRequest(pos) ⇒
      localPosition = pos
      context.become(common orElse tickHandling orElse taskHandling)
      unstashAll()
    case _ ⇒ stash()
  }

  def common: Receive = {
    case RegisterOwner(o)     ⇒ owner = Some(o)
    case ChangeOrientation(o) ⇒ orientation = o
  }

  val taskHandling: Receive

}

sealed trait BuildingType

object BuildingType {
  case object Restaurant extends BuildingType
  case object TownHall extends BuildingType
  case object Hospital extends BuildingType
  case object LivingQuarter extends BuildingType
}
