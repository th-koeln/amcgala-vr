package example

import scala.concurrent.ExecutionContext.Implicits.global
import org.amcgala.vr._
import org.amcgala.vr.Position
import example.LocationService.Coordinate
import org.amcgala.vr.building.BuildingType

object BresenhamIterator {

  def bresenham(x0: Int, y0: Int, x1: Int, y1: Int) = {
    import scala.math.abs

    val dx = abs(x1 - x0)
    val dy = abs(y1 - y0)

    val sx = if (x0 < x1) 1 else -1
    val sy = if (y0 < y1) 1 else -1

    new Iterator[Coordinate] {
      var (x, y) = (x0, y0)
      var err = dx - dy

      def next = {
        val point = Coordinate(x, y)
        val e2 = 2 * err
        if (e2 > -dy) {
          err -= dy
          x += sx
        }
        if (e2 < dx) {
          err += dx
          y += sy
        }
        point
      }

      def hasNext = !(x == x1 && y == y1)
    }
  }

}

object LocationService {
  case class Coordinate(x: Int, y: Int)
  case class Cell(id: String)

  class FindLocationTask(val buildingType: BuildingType)(implicit val bot: Bot) extends Task {
    type Return = Coordinate
    import scala.concurrent._

    def isDone() = true

    def execute(): Future[Return] = future { Coordinate(150, 150) }
  }

  class WalkToTask(coordinate: Coordinate)(implicit val bot: Bot) extends MultiStepTask {
    import scala.concurrent._
    type Return = LocationService.Cell

    private var p = BresenhamIterator.bresenham(0, 0, 0, 0)

    for (pos ← bot.position()) {
      p = BresenhamIterator.bresenham(pos.x.toInt, pos.y.toInt, coordinate.x, coordinate.y)
    }

    override def onTick(): Unit = {
      if (p.hasNext) {
        val n = p.next
        bot.moveToPosition(Position(n.x, n.y))
      } else {
        bot.moveToPosition(Position(coordinate.x, coordinate.y))
        done()
        result success LocationService.Cell("Yay")
      }
    }

    override def execute(): Future[Return] = result.future
  }

  def findLocation(buildingType: BuildingType)(implicit bot: Bot) = new FindLocationTask(buildingType)
  def walkTo(pos: Coordinate)(implicit bot: Bot) = new WalkToTask(pos)
}

