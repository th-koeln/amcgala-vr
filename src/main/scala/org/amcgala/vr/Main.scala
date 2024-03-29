package org.amcgala.vr

import akka.actor.{Actor, PoisonPill, ActorRef}
import akka.util.Timeout
import org.amcgala.vr.building.{Restaurant, BuildingType, TownHall}

import scala.util.Random
import example.{BresenhamIterator, LocationService}
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

  simulation.spawnBuilding(classOf[Restaurant], Coordinate(50, 50), BuildingType.Restaurant)

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
      hall ← bot.townHallCoordinate
      t ← bot.executeTask(LocationService.walkTo(Coordinate(hall.x, hall.y))(bot))
    } yield {
      done = true
      Unit
    }
  }
}

class JobBehavior()(implicit val bot: Bot) extends SatisfactionBehavior {

  import akka.pattern.ask
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  implicit val timeout = Timeout(5.seconds)

  type Return = Cell

  def start() = {
    for {
      pos ← bot.townHallCoordinate // Wo ist die TownHall?
      thp ← bot.executeTask(LocationService.walkTo(pos)(bot)) // Gehe zur TownHall
      v ← bot.vicinity(1) // Schau dir die Umgebung an, wenn du da bist
      thInfoOpt = v.buildings.find(_._2._2 == BuildingType.TownHall) // Versuch in deiner Nähe die TownHall zu finden
    } yield {
      for{
        thInfo <- thInfoOpt // Hol die Infos der TownHall
        th = thInfo._1 // Wir brauchen nur die ActorRef
        restaurants <- (th ? TownHall.BuildingsByTypeRequest(BuildingType.Restaurant, bot.ref)).mapTo[Set[ActorRef]] // Frag  nach Restaurants
      }{
        println(restaurants) // Gib die Restaurants aus, die wir gefunden haben
      }
      done = true
      thp
    }
  }

  val need: Need = Hunger()
}