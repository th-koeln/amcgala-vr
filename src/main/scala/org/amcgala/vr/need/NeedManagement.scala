package org.amcgala.vr.need

import org.amcgala.vr.Behavior
import org.amcgala.vr.need.Needs.NeedIDs.{HungerID, NeedID}

trait SatisfactionBehavior extends Behavior {
  val need: Need
}

trait Need {
  val id: NeedID
  var needManager: Option[NeedManager] = None

  var value: Double

  def addSatisfactionBehavior(behavior: SatisfactionBehavior): Unit

  def satisfactionBehaviors: List[SatisfactionBehavior]

  def increase(): Double

  def decrease(): Double

  def increase(delta: Double): Double

  def decrease(delta: Double): Double

  def update(): Unit
}

object Needs {

  object NeedIDs {
    sealed trait NeedID
    case object HungerID extends NeedID
  }

  class Hunger extends Need {
    val id = HungerID
    def addSatisfactionBehavior(behavior: SatisfactionBehavior): Unit = ???

    def satisfactionBehaviors: List[SatisfactionBehavior] = ???

    def decrease(): Double = ???

    def decrease(delta: Double): Double = ???

    def increase(): Double = ???

    def increase(delta: Double): Double = ???

    def update(): Unit = ???

    var value: Double = 0.0
  }

  object Hunger {
    def apply(): Hunger = new Hunger()

    val ID = NeedIDs.HungerID
  }

}

class NeedManager {
  private var needList = List.empty[Need]

  def removeNeed(id: NeedID): Unit = {
    needList = needList.filterNot(_.id == id)
  }

  def getSatisfactionStrategyFor(need: Need): SatisfactionBehavior = {
    // TODO Gibt ein Verhalten zurück, das vom Brain ausgeführt werden soll.
    null
  }

  def needSuggestion: List[Need] = {
    // TODO Zusammenstellen einer Liste von Bedürfnissen, die befriedigt werden müssen.
    null
  }

  def registerNeed(need: Need): Unit = {
    // Zur Liste hinzufügen
    need.needManager = Some(this)
    // TODO Hinzufügen eines neues Bedürfnis zu der Liste aller Bedürfnisse
  }

  def update(): Unit = {
    // TODO Aktualisieren aller Needs, die vom Manager verwaltet werden
  }
}
