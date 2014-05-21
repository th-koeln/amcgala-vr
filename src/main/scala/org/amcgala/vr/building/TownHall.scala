package org.amcgala.vr.building

import akka.actor.ActorRef

object TownHall {

  case object RegisterBot

  case object UnregisterBot

  case object RegisterBuilding

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
