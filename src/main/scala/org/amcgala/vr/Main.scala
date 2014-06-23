package org.amcgala.vr

import akka.actor.{Actor, PoisonPill, ActorRef}

import scala.util.Random
import example.{ BresenhamIterator, LocationService }
import scala.concurrent.Future
import org.amcgala.vr.building.BuildingType.Restaurant
import org.amcgala.CellType
import org.amcgala.vr.need.{Need, SatisfactionBehavior}
import org.amcgala.vr.need.Needs.Hunger

/**
  * Startet die Simulation.
  */
object Main extends App {
  val simulation = new Simulation(200, 200)

  for (i ← 0 until 15) {
    simulation.spawnBot(classOf[SimpleNPC], Coordinate(Random.nextInt(simulation.width), Random.nextInt(simulation.height)))
  }

  for (x ← 50 until 150) {
    simulation.changeCellType(Coordinate(x, 98), CellType.Road)
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

  def start(): Future[Return] = {
    for {
      hall <- bot.townHall
      t ← bot.executeTask(LocationService.walkTo(Coordinate(hall.x, hall.y))(bot))
    } yield {
      done = true
      Unit
    }
  }
}



class JobBehavior()(implicit val bot: Bot) extends SatisfactionBehavior {

  import scala.concurrent.ExecutionContext.Implicits.global

  type Return = Cell


  def start() = {
    for {
      pos ← bot.executeTask(LocationService.findLocation(Restaurant)(bot))
      mcd ← bot.executeTask(LocationService.walkTo(pos)(bot))
      cells <- bot.visibleCells(1.5)
    } yield {
      println(cells)
      done = true
      need.decrease(49)
      mcd
    }
  }

  val need: Need = Hunger()
}