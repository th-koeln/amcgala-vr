package org.amcgala.vr

import akka.actor.{ PoisonPill, ActorRef, ActorSystem, Props }
import scala.util.Random
import org.amcgala.{ RGBColor, Scene, FrameworkMode, Framework }
import java.awt.Color
import org.amcgala.shape.Rectangle
import org.amcgala.math.Vertex3f

/**
  * The absolute position of an entity in the [[Simulation]].
  * @param x
  * @param y
  */
case class Position(x: Double, y: Double)

/**
 * A Cell in the simulated world map.
 * @param cellType the [[CellTypes.CellType]] of the Cell.
 */
case class Cell(cellType: CellTypes.CellType)

object SimulationAgent {

  case class Register(bot: ActorRef, position: Position)

  case object Unregister

  case class ChangeCellType(position: Position, cellType: CellTypes.CellType)

  case class ChangePosition(position: Position)

  case class PositionRequest(ref: ActorRef)

  case class CellRequest(ref: ActorRef)

  case class VicinityRequest(ref: ActorRef, distance: Double)

  def props(width: Int, height: Int) = Props(new SimulationAgent(width, height))
}

class SimulationAgent(val width: Int, val height: Int) extends Agent {

  import SimulationAgent._

  var field = (for {
    x ← 0 until width
    y ← 0 until height
  } yield Position(x, y) -> Cell(CellTypes.Floor)).toMap

  var positions = Map[ActorRef, Position]()
  val framework = Framework.getInstance(FrameworkMode.SOFTWARE)
  val scene = new Scene("vis")
  val scaleX = framework.getWidth / width
  val scaleY = framework.getHeight / height

  val rectangles = (for {
    x ← 0 until width
    y ← 0 until height
  } yield {
    val rect = new Rectangle(new Vertex3f(x * scaleX, y * scaleY, -1), scaleX, scaleY)
    rect.setColor(RGBColor.BLACK)
    scene.addShape(rect)
    Position(x, y) -> rect
  }).toMap

  framework.loadScene(scene)

  registerOnTickAction(() ⇒ {
    for {
      f ← field
      r ← rectangles.get(f._1)
    } {
      f._2.cellType match {
        case CellTypes.Floor     ⇒ r.setColor(RGBColor.WHITE)
        case CellTypes.Forbidden ⇒ r.setColor(RGBColor.RED)
        case CellTypes.Wall      ⇒ r.setColor(RGBColor.BLACK)
      }
    }

    for {
      f ← positions
      r ← rectangles.get(f._2)
    } {
      r.setColor(RGBColor.GREEN)
    }
  })

  context.become(receive orElse tickHandling)

  def receive: Receive = {
    case Register(bot, position) ⇒
      if (field.exists(_._1 == position)) {
        if (field(position).cellType != CellTypes.Forbidden) {
          bot ! Bot.Introduction
          bot ! Bot.PositionChange(position)
          positions = positions + (bot -> position)
        }
      } else {
        bot ! PoisonPill
      }

    case ChangeCellType(position, cellType) ⇒
      if (field.exists(_._1 == position)) {
        field = field + (position -> Cell(cellType))
      }

    case ChangePosition(position) ⇒
      if (field.exists(_._1 == position)) {
        if (field(position).cellType != CellTypes.Forbidden) {
          positions = positions + (sender() -> position)
          sender() ! Bot.PositionChange(position)
        }
      }

    case PositionRequest(ref) ⇒
      for (pos ← positions.get(ref)) {
        sender() ! pos
      }

    case VicinityRequest(ref, dis) ⇒
      val pos = positions(ref)
      sender() ! positions.filter(t ⇒ Utils.distance(pos, t._2) < dis && t._1 != ref)

    case CellRequest(ref) ⇒
      val pos = positions(ref)
      sender() ! field(pos)

    case Unregister ⇒
      positions = positions - sender()

  }

}

class Simulation(val width: Int, val height: Int)(implicit system: ActorSystem) {

  import SimulationAgent._

  private val heartbeat = system.actorOf(HeartBeat.props())
  private val sim = system.actorOf(SimulationAgent.props(width, height))

  /**
    * Erzeugt einen neuen Bot und gibt die Referenz zurück.
    * @param cls die Klasse des Bots
    * @param position die Position des Bots
    * @tparam T der Typ des Bots. Muss eine Unterklasse von [[Bot]] sein
    * @return [[ActorRef]] des Bots
    */
  def spawnBot[T <: Bot](cls: Class[T], position: Position): ActorRef = {
    val bot = system.actorOf(Props(cls))
    sim ! Register(bot, position)
    bot
  }

  def spawnBot[T <: Bot](cls: Class[T]): ActorRef = {
    val bot = system.actorOf(Props(cls))
    sim ! Register(bot, randomPosition())
    bot
  }

  def changeCellType(position: Position, cellType: CellTypes.CellType) = {
    sim ! ChangeCellType(position, cellType)
  }

  def randomPosition(): Position = Position(Random.nextDouble() * width, Random.nextDouble() * height)
}

