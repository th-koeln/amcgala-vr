package org.amcgala.vr

import scala.util.Random
import example.{Restaurant, BresenhamIterator, LocationService}
import example.LocationService.Coordinate
import scala.concurrent.Future

/**
  * Startet die Simulation.
  */
object Main extends App {
  val simulation = new Simulation(200, 200)

  //  for (y ← 0 until simulation.height / 2) {
  //    simulation.changeCellType(GridIndex(simulation.width / 2, y), CellTypes.Forbidden)
  //  }

  for (i ← 0 until 15) {
    simulation.spawnBot(classOf[SimpleNPC], Position(Random.nextInt(simulation.width), Random.nextInt(simulation.height)))
  }
}

class SimpleNPC extends BotAgent with BrainModule {

  brain.registerJob(new JobBehavior())
  brain.registerIdleBehavior(new RandomWalkBehavior())
}

class RandomWalkBehavior()(implicit val bot: Bot) extends Behavior {
  import scala.concurrent.ExecutionContext.Implicits.global

  type Return = Unit.type
  private val target = Coordinate(Random.nextInt(200), Random.nextInt(200))
  private var path = BresenhamIterator.bresenham(0,0,0,0)


  private var done = false

  def isDone(): Boolean = done

  def start(): Future[Return] = {
   for(t <- bot.executeTask(LocationService.walkTo(target)(bot))) yield {
     done = true
     Unit
   }
  }


}

class JobBehavior()(implicit val bot: Bot) extends Behavior {
  import scala.concurrent.ExecutionContext.Implicits.global
  type Return = LocationService.Cell
  private var done = false


  def isDone(): Boolean = done

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