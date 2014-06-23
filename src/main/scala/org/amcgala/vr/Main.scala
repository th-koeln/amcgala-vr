package org.amcgala.vr

import akka.actor.{Actor, PoisonPill, ActorRef}
import org.amcgala.vr.building.TownHall

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
      pos <- bot.townHall
      thp ← bot.executeTask(LocationService.walkTo(pos)(bot))
      v <- bot.vicinity(1)
      cells <- bot.visibleCells(2)
    } yield {
      val th = v.buildings.find(_._1.path.toString.contains("town"))
      th map (_._1 ! TownHall.RegisterBot)
      done = true
      thp
    }
  }

  val need: Need = Hunger()
}