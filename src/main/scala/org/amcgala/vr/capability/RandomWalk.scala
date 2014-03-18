package org.amcgala.vr.capability

import scala.util.Random
import org.amcgala.vr.Bot

/**
  * Lets the [[Bot]] walk randomly.
  */
trait RandomWalk extends BotCapability {
  bot: Bot ⇒

  registerOnTickAction(() ⇒ {
    val turns = Random.nextInt(3)
    val left = Random.nextBoolean()

    if (left) {
      for (i ← 0 to turns) {
        turnLeft()
      }
    } else {
      for (i ← 0 to turns) {
        turnRight()
      }
    }
    moveForward()
  })
}