package org.amcgala.vr

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

class NeedManager {
  def getSatisfactionStrategyFor(need: Need): SatisfactionBehavior = ???
  def needSuggestion: List[Need] = ???
  def registerNeed(need: Need) = ???
  def update(): Unit = {}
}
