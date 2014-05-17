package org.amcgala.vr.capability

import org.amcgala.vr.BotAgent
import akka.actor.PoisonPill

/**
  * Adds health to the [[BotAgent]]. If the health of a Bot is lower than 0, the Bot dies and is removed from the [[org.amcgala.vr.Simulation]].
  */
trait Health extends BotCapability {
  bot: BotAgent ⇒

  var health: Int = 100

  registerOnTickAction("healthHandling", () ⇒ {
    if (health < 0) {
      self ! PoisonPill
    }
  })
}