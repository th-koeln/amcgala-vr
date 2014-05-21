package org.amcgala.vr

import akka.actor.{ PoisonPill, ActorRef, ActorSystem, Props }
import scala.util.Random
import org.amcgala._
import org.amcgala.shape.Rectangle
import org.amcgala.math.Vertex3f
import org.amcgala.vr.building.Building

/**
  * The absolute position of an entity in the [[Simulation]].
  * @param x
  * @param y
  */
case class Position(x: Int, y: Int)

/**
  * A Cell in the simulated world map.
  * @param cellType the [[CellType]] of the Cell.
  */
case class Cell(cellType: CellType)

object SimulationAgent {

  /**
    * Registers a new [[BotAgent]] with the [[SimulationAgent]].
    * @param bot the [[ActorRef]] of the [[BotAgent]]
    * @param position the [[Position]] in the simulated world
    */
  case class RegisterAgent(bot: ActorRef, position: Position)

  case class RegisterBuilding(bot: ActorRef, position: Position)

  /**
    * Removes a [[BotAgent]] from the [[SimulationAgent]]
    */
  case object Unregister

  /**
    * Changes the [[CellType]] of a Cell.
    * @param index the [[Position]] of the [[Cell]] in the world
    * @param cellType the new [[CellType]]
    */
  case class CellTypeChange(index: Position, cellType: CellType)

  /**
    * Changes the [[Position]] of a [[BotAgent]].
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

  case class CellAtIndexRequest(index: Position)

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

  val field = Array.fill(width, height)(Cell(CellType.Floor))

  var agentPositions = Map[ActorRef, Position]()
  var buildingPositions = Map[ActorRef, Position]()

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

  registerOnTickAction("drawing", () ⇒ {

    for {
      x ← 0 until width
      y ← 0 until height
    } {
      val cell = field(x)(y)
      rectangles(x)(y).setColor(cell.cellType.color)
    }

    val posIt = agentPositions.iterator

    while (posIt.hasNext) {
      val next = posIt.next()
      rectangles(next._2.x)(next._2.y).setColor(RGBColor.GREEN)
    }

    val buildingsIt = buildingPositions.iterator

    while (buildingsIt.hasNext) {
      val next = buildingsIt.next()
      rectangles(next._2.x)(next._2.y).setColor(RGBColor.BLUE)
    }

  })

  def receive: Receive = {
    case RegisterAgent(bot, position) ⇒
      if (position.x >= 0 && position.x < width && position.y >= 0 && position.y < height) {
        if (field(position.x)(position.y).cellType != CellType.Forbidden) {
          bot ! BotAgent.Introduction
          bot ! BotAgent.PositionChange(position)
          agentPositions = agentPositions + (bot -> position)
        }
      } else {
        bot ! PoisonPill
      }

    case RegisterBuilding(ref, position) ⇒
      if (position.x >= 0 && position.x < width && position.y >= 0 && position.y < height) {
        if (field(position.x)(position.y).cellType != CellType.Forbidden) {
          ref ! BotAgent.Introduction
          ref ! BotAgent.PositionChange(position)
          buildingPositions = buildingPositions + (ref -> position)
        }
      } else {
        ref ! PoisonPill
      }

    case CellTypeChange(gridIdx, cellType) ⇒
      if (gridIdx.x >= 0 && gridIdx.x < width && gridIdx.y >= 0 && gridIdx.y < height) {
        field(gridIdx.x)(gridIdx.y) = Cell(cellType)
      }

    case PositionChange(position) ⇒
      if (position.x >= 0 && position.x < width && position.y >= 0 && position.y < height) {
        if (field(position.x)(position.y).cellType != CellType.Forbidden) {
          agentPositions = agentPositions + (sender() -> position)
          sender() ! BotAgent.PositionChange(position)
        }
      }

    case PositionRequest(ref) ⇒
      for (pos ← agentPositions.get(ref)) {
        sender() ! pos
      }

    case VicinityRequest(ref, dis) ⇒
      val pos = agentPositions(ref)
      sender() ! agentPositions.filter(t ⇒ Utils.distance(pos, t._2) < dis && t._1 != ref)

    case CellRequest(ref) ⇒
      val position = agentPositions(ref)
      sender() ! field(position.x)(position.y)

    case CellAtIndexRequest(idx) ⇒
      if (idx.x >= 0 && idx.x < field.length && idx.y >= 0 && idx.y < field(0).length) {
        sender() ! field(idx.x)(idx.y)
      }
    case Unregister ⇒
      agentPositions = agentPositions - sender()

  }

}

class Simulation(val width: Int, val height: Int)(implicit system: ActorSystem) {

  import SimulationAgent._

  private val heartbeat = system.actorOf(HeartBeat.props())
  private val sim = system.actorOf(SimulationAgent.props(width, height))

  /**
    * Creates a new instance of a [[BotAgent]].
    * @param cls the class of the bot, must be a subclass of [[BotAgent]]
    * @param position the starting position of the bot
    * @tparam T the class type of this bot
    * @return [[ActorRef]]
    */
  def spawnBot[T <: BotAgent](cls: Class[T], position: Position): Bot = {
    val bot = system.actorOf(Props(cls))
    sim ! RegisterAgent(bot, position)
    Bot(bot)
  }

  def spawnBot[T <: BotAgent](cls: Class[T]): Bot = {
    val bot = system.actorOf(Props(cls))
    sim ! RegisterAgent(bot, randomPosition())
    Bot(bot)
  }

  def spawnBuilding[T <: Building](cls: Class[T], position: Position): ActorRef = {
    val building = system.actorOf(Props(cls))
    sim ! RegisterBuilding(building, position)
    building
  }

  def changeCellType(index: Position, cellType: CellType) = {
    sim ! CellTypeChange(index, cellType)
  }

  def randomPosition(): Position = Position(Random.nextInt(width), Random.nextInt(height))
}

