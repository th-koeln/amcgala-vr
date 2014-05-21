package org.amcgala.vr

import scala.util.Random
import example.{ BresenhamIterator, LocationService }
import example.LocationService.Coordinate
import scala.concurrent.Future
import org.amcgala.vr.building.TownHall
import org.amcgala.vr.building.BuildingType.Restaurant
import org.amcgala.CellType

/**
  * Startet die Simulation.
  */
object Main extends App {
  val simulation = new Simulation(200, 200)

  for (i ← 0 until 15) {
    simulation.spawnBot(classOf[SimpleNPC], Position(Random.nextInt(simulation.width), Random.nextInt(simulation.height)))
  }

  simulation.spawnBuilding(classOf[TownHall], Position(100, 100))
  for (x ← 50 until 150) {
    simulation.changeCellType(Position(x, 98), CellType.Road)
  }
}

class SimpleNPC extends BotAgent {
  brain.registerJob(new JobBehavior())
  brain.registerIdleBehavior(new RandomWalkBehavior())
}

class RandomWalkBehavior()(implicit val bot: Bot) extends Behavior {

  import scala.concurrent.ExecutionContext.Implicits.global

  type Return = Unit.type
  private val target = Coordinate(Random.nextInt(200), Random.nextInt(200))
  private var path = BresenhamIterator.bresenham(0, 0, 0, 0)

  def start(): Future[Return] = {
    for (t ← bot.executeTask(LocationService.walkTo(target)(bot))) yield {
      done = true
      Unit
    }
  }
}

class JobBehavior()(implicit val bot: Bot) extends Behavior {

  import scala.concurrent.ExecutionContext.Implicits.global

  type Return = LocationService.Cell

  def start() = {
    for {
      pos ← bot.executeTask(LocationService.findLocation(Restaurant)(bot))
      mcd ← bot.executeTask(LocationService.walkTo(pos)(bot))
    } yield {
      done = true
      mcd
    }
  }
}