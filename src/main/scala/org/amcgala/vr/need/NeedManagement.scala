package org.amcgala.vr.need

import org.amcgala.vr.Behavior
import org.amcgala.vr.need.Needs.NeedIDs.NeedID

trait SatisfactionBehavior extends Behavior {
  val need: Need
}

trait Need {

  def addSatisfactionBehavior(behavior: SatisfactionBehavior): Unit

  def satisfactionBehaviors: List[SatisfactionBehavior]

  def increase(): Double

  def decrease(): Double

  def increase(delta: Double): Double

  def decrease(delta: Double): Double
}

object Needs {

  object NeedIDs {
    sealed trait NeedID
    case object HungerID extends NeedID
  }

  class Hunger extends Need {
    def addSatisfactionBehavior(behavior: SatisfactionBehavior): Unit = ???

    def satisfactionBehaviors: List[SatisfactionBehavior] = ???

    def decrease(): Double = ???

    def decrease(delta: Double): Double = ???

    def increase(): Double = ???

    def increase(delta: Double): Double = ???
  }

  object Hunger {
    def apply(): Hunger = new Hunger()

    val ID = NeedIDs.HungerID
  }

}

class NeedManager {
  def removeNeed(id: NeedID): Unit = {
    // TODO Hier wird ein Bedürfnis wieder aus dem Manager entfernt.
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
    // TODO Hinzufügen eines neues Bedürfnis zu der Liste aller Bedürfnisse
  }

  def update(): Unit = {
    // TODO Aktualisieren aller Needs, die vom Manager verwaltet werden
  }
}
