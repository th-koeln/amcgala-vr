package org.amcgala.vr.capability

import org.amcgala.vr.Bot
import akka.actor.PoisonPill

/**
  * Adds health to the [[Bot]]. If the health of a Bot is lower than 0, the Bot dies and is removed from the [[org.amcgala.vr.Simulation]].
  */
trait Health extends BotCapability {
  bot: Bot ⇒

  var health: Int = 100

  registerOnTickAction(() ⇒ {
    if (health < 0) {
      self ! PoisonPill
    }
  })
}