package org.amcgala.vr.capability

import org.amcgala.vr.{Cell, Position, Bot}

/**
 * The ability of a [[org.amcgala.vr.Bot]] to memorize already visited cell. This knowledge can be used for navigation 
 * tasks.
 */
trait LocalMap extends BotCapability{
  bot: Bot =>
  var currentPosition = localPosition
  var knownCells = Map[Position, Cell]()
  
  registerOnTickAction(() => {
    if(currentPosition != localPosition) {
      currentPosition = localPosition
      for(cell <- cell()){
        knownCells = knownCells + (currentPosition -> cell)
      }
    }
  })
}
