package org.amcgala.vr.building

import akka.actor.{Props, ActorRef}

object TownHall {

  trait TownHallMessage

  case object RegisterBot extends TownHallMessage

  case object UnregisterBot extends TownHallMessage

  case object RegisterBuilding extends TownHallMessage

  def props() = Props(new TownHall)

  // TODO Weitere Nachrichten, die von dem Dorfzentrum bearbeitet werden sollen hier ergänzen.
}

class TownHall extends Building {

  import TownHall._

  var knownBots = Set[ActorRef]()
  var knownBuildings = Set[ActorRef]()

  def taskHandling: Receive = {
    case RegisterBot      ⇒
    case UnregisterBot    ⇒
    case RegisterBuilding ⇒
  }
}
