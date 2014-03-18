package org.amcgala.vr.capability

import org.amcgala.vr.Bot

/**
  * A simple Hunger definition for a [[Bot]]. It depends on the [[Health]] [[BotCapability]].
  */
trait Hunger extends BotCapability {
  bot: Bot with Health ⇒

  var hunger: Int = 0
  var ticks: Int = 0

  registerOnTickAction(() ⇒ {
    ticks = (ticks + 1) % 1

    if (ticks == 0) {
      hunger += 1
    }

    if (hunger > 100 && ticks == 0) {
      health -= 1
    }
  })
}