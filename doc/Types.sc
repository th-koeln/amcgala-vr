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
  val bot: Bot with BrainModule
  def start()
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
    type Return = Cell

    def execute(): Future[Return] = Future.successful(Cell("McD"))
  }


  def findLocation(buildingType: BuildingType) = new FindLocationTask(buildingType)
  def walkTo(pos: Coordinate) = new GotoTask(pos)
}

class FindFriesBehavior(val bot: Bot with BrainModule) extends Behavior{
  def start(): Unit = {
    for{
      pos <- bot.brain.executeTask(LocationService.findLocation(Restaurant))
      mcd <- bot.brain.executeTask(LocationService.walkTo(pos))
    }{
      println(pos.x)
    }
  }
}

val b = new Bot with BrainModule

new FindFriesBehavior(b).start()

