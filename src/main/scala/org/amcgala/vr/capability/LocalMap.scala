package org.amcgala.vr.capability

import org.amcgala.vr.{ Cell, Position, BotAgent }

/**
  * The ability of a [[org.amcgala.vr.BotAgent]] to memorize already visited cell. This knowledge can be used for navigation
  * tasks.
  */
trait LocalMap extends BotCapability {
  bot: BotAgent ⇒
  var currentPosition = localPosition
  var knownCells = Map[Position, Cell]()

  registerOnTickAction("localMap", () ⇒ {
    if (currentPosition != localPosition) {
      currentPosition = localPosition
      for (cell ← cell()) {
        knownCells = knownCells + (currentPosition -> cell)
      }
    }
  })
}
