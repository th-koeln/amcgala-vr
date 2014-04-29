package example

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait Task{
  type Return
  def execute(): Future[Return]
}

class Bot

class Brain{
  def executeTask(task: Task): Future[task.Return] = task.execute()
  def executeBehavior(behavior: Behavior) = behavior.start()
}

trait BrainModule{self: Bot =>
  val brain = new Brain
}

trait Behavior{
  type Return
  val bot: Bot with BrainModule
  def start(): Future[Return]
}



sealed trait BuildingType
case object Restaurant extends BuildingType

object LocationService{
  case class Coordinate(x: Int, y: Int)
  case class Cell(id: String)

  class FindLocationTask(val buildingType: BuildingType) extends Task{
    type Return = Coordinate

    def execute(): Future[Return] = Future.successful(Coordinate(0,0))
  }

  class GotoTask(coordinate: Coordinate) extends Task{
    type Return = LocationService.Cell

    def execute(): Future[Return] = Future.successful(Cell("McD"))
  }


  def findLocation(buildingType: BuildingType) = new FindLocationTask(buildingType)
  def walkTo(pos: Coordinate) = new GotoTask(pos)
}

class FindFriesBehavior(val bot: Bot with BrainModule) extends Behavior{
  type Return = LocationService.Cell

  def start() = {
    println("starting")
    for{
      pos <- bot.brain.executeTask(LocationService.findLocation(Restaurant))
      mcd <- bot.brain.executeTask(LocationService.walkTo(pos))
    } yield mcd
  }
}

object Test extends App{

  val b = new Bot with BrainModule

  println("running task")
  val c = new FindFriesBehavior(b).start()

  for(cc <- c) println(cc.id)

  Thread.sleep(2000)

}