package org.amcgala.vr

/**
  * Startet die Simulation.
  */
object Main extends App {
  val simulation = new Simulation(100, 100)

  for (y ← 0 until simulation.height / 2) {
    simulation.changeCellType(Position(simulation.width / 2, y), CellTypes.Forbidden)
  }

  simulation.spawnBot(classOf[TestAgent], Position(20, 20))
  simulation.spawnBot(classOf[TestAgent], Position(10, 20))
}

class TestAgent extends Bot with Health with Hunger with RandomWalk {

  override def customBehaviour(): Unit = {
    for {
      vic ← vicinity(5)
      n ← vic.headOption
      h ← requestHeading(n._1)
    } {
      println(h)
    }
  }
}
