package org.amcgala.vr.capability

import org.amcgala.vr.Bot

/**
  * A simple Hunger definition for a [[Bot]]. It depends on the [[Health]] [[BotCapability]].
  */
trait Hunger extends BotCapability {
  bot: Bot with Health ⇒

  var hunger: Int = 0
  var counter: Int = 0

  registerOnTickAction(() ⇒ {
    counter = (counter + 1) % 5

    if (counter == 0) {
      hunger += 1
    }

    if (hunger > 100 && counter == 0) {
      health -= hunger - 100
    }
  })
}