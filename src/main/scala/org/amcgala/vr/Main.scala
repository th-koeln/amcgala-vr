package org.amcgala.vr

import org.amcgala.vr.capability.{ Health, Hunger, RandomWalk }

/**
  * Startet die Simulation.
  */
object Main extends App {
  val simulation = new Simulation(100, 100)

  for (y ← 0 until simulation.height / 2) {
    simulation.changeCellType(Position(simulation.width / 2, y), CellTypes.Forbidden)
  }

  simulation.spawnBot(classOf[SimpleNPC], Position(20, 20))
  simulation.spawnBot(classOf[SimpleNPC], Position(10, 20))
}

class SimpleNPC extends Bot with Health with Hunger with RandomWalk
