package org.amcgala.vr

import akka.actor.{ PoisonPill, ActorRef, ActorSystem, Props }
import scala.util.Random
import org.amcgala.{ RGBColor, Scene, FrameworkMode, Framework }
import org.amcgala.shape.Rectangle
import org.amcgala.math.Vertex3f

/**
  * The absolute position of an entity in the [[Simulation]].
  * @param x
  * @param y
  */
case class Position(x: Double, y: Double)

case class GridIndex(x: Int, y: Int)

/**
  * A Cell in the simulated world map.
  * @param cellType the [[CellTypes.CellType]] of the Cell.
  */
case class Cell(cellType: CellTypes.CellType)

object SimulationAgent {

  /**
    * Registers a new [[Bot]] with the [[SimulationAgent]].
    * @param bot the [[ActorRef]] of the [[Bot]]
    * @param position the [[Position]] in the simulated world
    */
  case class Register(bot: ActorRef, position: Position)

  /**
    * Removes a [[Bot]] from the [[SimulationAgent]]
    */
  case object Unregister

  /**
    * Changes the [[CellTypes.CellType]] of a Cell.
    * @param gridIdx the [[Position]] of the [[Cell]] in the world
    * @param cellType the new [[CellTypes.CellType]]
    */
  case class CellTypeChange(gridIdx: GridIndex, cellType: CellTypes.CellType)

  /**
    * Changes the [[Position]] of a [[Bot]].
    * @param position the new position
    */
  case class PositionChange(position: Position)

  /**
    * A position request from someone. The [[SimulationAgent]]'s response is the current position of the Bot.
    * @param ref the [[ActorRef]] of the Bot
    */
  case class PositionRequest(ref: ActorRef)

  /**
    * The SimulationAgent answers this message with a [[Cell]] instance if the [[ActorRef]] is known.
    * @param ref the [[ActorRef]] of the requesting Bot
    */
  case class CellRequest(ref: ActorRef)

  case class CellAtIdxRequest(gridIdx: GridIndex)

  /**
    * The SimulationAgent answers this message with a List of ([[ActorRef]], [[Position]]) Tuples of all Bots that are
    * in the vicinity of the requesting Bot.
    * @param ref the [[ActorRef]] of the requesting Bot
    * @param distance the radius of the vicinity
    */
  case class VicinityRequest(ref: ActorRef, distance: Double)

  def props(width: Int, height: Int) = Props(new SimulationAgent(width, height))
}

class SimulationAgent(val width: Int, val height: Int) extends Agent {

  import SimulationAgent._

  val field = Array.fill(width, height)(Cell(CellTypes.Floor))

  var positions = Map[ActorRef, Position]()
  val framework = Framework.getInstance(FrameworkMode.SOFTWARE)
  val scene = new Scene("vis")
  val scaleX = framework.getWidth / width
  val scaleY = framework.getHeight / height

  val rectangles = Array.ofDim[Rectangle](width, height)
  for {
    x ← 0 until width
    y ← 0 until height
  } {
    val rect = new Rectangle(new Vertex3f(x * scaleX, y * scaleY, -1), scaleX, scaleY)
    rect.setColor(RGBColor.BLACK)
    scene.addShape(rect)
    rectangles(x)(y) = rect
  }

  framework.loadScene(scene)

  registerOnTickAction(() ⇒ {

    for {
      x ← 0 until width
      y ← 0 until height
    } {
      val cell = field(x)(y)
      rectangles(x)(y).setColor(cell.cellType.color)
    }

    val posIt = positions.iterator

    while (posIt.hasNext) {
      val next = posIt.next()
      rectangles(math.round(next._2.x).toInt)(math.round(next._2.y).toInt).setColor(RGBColor.GREEN)
    }

  })

  def receive: Receive = {
    case Register(bot, position) ⇒
      if (position.x >= 0 && position.x < width && position.y >= 0 && position.y < height) {
        if (field(math.round(position.x).toInt)(math.round(position.y).toInt).cellType != CellTypes.Forbidden) {
          bot ! Bot.Introduction
          bot ! Bot.PositionChange(position)
          positions = positions + (bot -> position)
        }
      } else {
        bot ! PoisonPill
      }

    case CellTypeChange(gridIdx, cellType) ⇒
      if (gridIdx.x >= 0 && gridIdx.x < width && gridIdx.y >= 0 && gridIdx.y < height) {
        field(gridIdx.x)(gridIdx.y) = Cell(cellType)
      }

    case PositionChange(position) ⇒
      if (position.x >= 0 && position.x < width && position.y >= 0 && position.y < height) {
        if (field(math.round(position.x).toInt)(math.round(position.y).toInt).cellType != CellTypes.Forbidden) {
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
      val position = positions(ref)
      sender() ! field(math.round(position.x).toInt)(math.round(position.y).toInt)

    case CellAtIdxRequest(idx) ⇒
      if (idx.x >= 0 && idx.x < field.length && idx.y >= 0 && idx.y < field(0).length) {
        sender() ! field(idx.x)(idx.y)
      }
    case Unregister ⇒
      positions = positions - sender()

  }

}

class Simulation(val width: Int, val height: Int)(implicit system: ActorSystem) {

  import SimulationAgent._

  private val heartbeat = system.actorOf(HeartBeat.props())
  private val sim = system.actorOf(SimulationAgent.props(width, height))

  /**
    * Creates a new instance of a [[Bot]].
    * @param cls the class of the bot, must be a subclass of [[Bot]]
    * @param position the starting position of the bot
    * @tparam T the class type of this bot
    * @return [[ActorRef]]
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

  def changeCellType(gridIdx: GridIndex, cellType: CellTypes.CellType) = {
    sim ! CellTypeChange(gridIdx, cellType)
  }

  def randomPosition(): Position = Position(Random.nextDouble() * width, Random.nextDouble() * height)
}

