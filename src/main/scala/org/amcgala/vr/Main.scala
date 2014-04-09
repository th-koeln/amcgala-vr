package org.amcgala.vr

import org.amcgala.vr.capability.{ Health, Hunger, RandomWalk }
import scala.util.Random

/**
  * Startet die Simulation.
  */
object Main extends App {
  val simulation = new Simulation(200, 200)

  for (y ← 0 until simulation.height / 2) {
    simulation.changeCellType(GridIndex(simulation.width / 2, y), CellTypes.Forbidden)
  }

  for (i ← 0 until 200) {
    simulation.spawnBot(classOf[SimpleNPC], Position(Random.nextInt(simulation.width), Random.nextInt(simulation.height)))
  }
}

class SimpleNPC extends Bot
  with Health with Hunger with RandomWalk
