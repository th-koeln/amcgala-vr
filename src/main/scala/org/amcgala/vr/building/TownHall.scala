package org.amcgala.vr.building

import akka.actor.{ Props, ActorRef }
import akka.util.Timeout
import org.amcgala.vr.{ Utils, Coordinate, SimulationAgent }

import scala.concurrent.Future

object TownHall {

  trait TownHallMessage

  case object RegisterBot extends TownHallMessage

  case object UnregisterBot extends TownHallMessage

  case class RegisterBuilding(buildingType: BuildingType) extends TownHallMessage

  case class BuildingsByTypeRequest(buildingType: BuildingType, requester: ActorRef) extends TownHallMessage

  def props() = Props(new TownHall)

  // TODO Weitere Nachrichten, die von dem Dorfzentrum bearbeitet werden sollen hier ergänzen.
}

class TownHall extends Building {

  import TownHall._
  import akka.util.Timeout
  import scala.concurrent.duration._
  import akka.pattern.ask

  implicit val ec = context.system.dispatcher
  implicit val timeout = Timeout(5.seconds)

  var knownBots = Set[ActorRef]()
  var knownBuildings = Map[BuildingType, Set[ActorRef]]()

  def isCloseEnough(ref: ActorRef): Future[Boolean] = {
    for {
      pos ← (simulation ? SimulationAgent.PositionRequest(ref)).mapTo[Coordinate]
      distance = Utils.manhattanDistance(localPosition, pos)
    } yield {
      distance < 3
    }
  }

  val taskHandling: Receive = {
    case RegisterBot ⇒
      knownBots += sender()
    case UnregisterBot ⇒
      knownBots -= sender()
    case RegisterBuilding(buildingType: BuildingType) ⇒
      knownBuildings += (buildingType -> (knownBuildings.getOrElse(buildingType, Set.empty[ActorRef]) + sender()))
    case BuildingsByTypeRequest(buildingType, ref) ⇒
      for (closeEnough ← isCloseEnough(ref)) {
        if (closeEnough) {
          for (buildings ← knownBuildings.get(buildingType)) {
            sender() ! buildings
          }
        }
      }
  }
}
