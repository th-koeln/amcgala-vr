package org.amcgala.vr

import akka.actor.{ PoisonPill, ActorRef, ActorSystem, Props }
import scala.util.Random
import org.amcgala._
import org.amcgala.shape.Rectangle
import org.amcgala.math.Vertex3f
import org.amcgala.vr.building.{TownHall, Building}

/**
  * The absolute position of an entity in the [[Simulation]].
  * @param x
  * @param y
  */
case class Coordinate(x: Int, y: Int)

/**
  * A Cell in the simulated world map.
  * @param cellType the [[CellType]] of the Cell.
  */
case class Cell(cellType: CellType)

object SimulationAgent {

  /**
    * Registers a new [[BotAgent]] with the [[SimulationAgent]].
    * @param bot the [[ActorRef]] of the [[BotAgent]]
    * @param position the [[Coordinate]] in the simulated world
    */
  case class RegisterAgentRequest(bot: ActorRef, position: Coordinate)

  case class RegisterBuildingRequest(bot: ActorRef, position: Coordinate)

  /**
    * Removes a [[BotAgent]] from the [[SimulationAgent]]
    */
  case object UnregisterRequest

  /**
    * Changes the [[CellType]] of a Cell.
    * @param index the [[Coordinate]] of the [[Cell]] in the world
    * @param cellType the new [[CellType]]
    */
  case class CellTypeChangeRequest(index: Coordinate, cellType: CellType)

  /**
    * Changes the [[Coordinate]] of a [[BotAgent]].
    * @param position the new position
    */
  case class PositionChangeRequest(position: Coordinate)

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

  case class CellAtIndexRequest(index: Coordinate)

  /**
    * The SimulationAgent answers this message with a List of ([[ActorRef]], [[Coordinate]]) Tuples of all Bots that are
    * in the vicinity of the requesting Bot.
    * @param ref the [[ActorRef]] of the requesting Bot
    * @param distance the radius of the vicinity
    */
  case class VicinityRequest(ref: ActorRef, distance: Int)

  case class VicinityReponse(bots: Map[ActorRef, Coordinate], buildings: Map[ActorRef, Coordinate])

  case class VisibleCellsRequest(ref: ActorRef, distance: Int)

  def props(width: Int, height: Int) = Props(new SimulationAgent(width, height))
}

class SimulationAgent(val width: Int, val height: Int) extends Agent {

  import SimulationAgent._

  var field = (for{
    x <- 0 until width
    y <- 0 until height
  } yield Coordinate(x,y) -> Cell(CellType.Floor)).toMap

  var agentPositions = Map[ActorRef, Coordinate]()
  var buildingPositions = Map[ActorRef, Coordinate]()

  val townHall = context.actorOf(TownHall.props(), "town-hall")
  val townHallLocation = Coordinate(100,100)
  self ! RegisterBuildingRequest(townHall, townHallLocation)



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

   for(e <- field){
     val coordinate = e._1
     val cell = e._2
     rectangles(coordinate.x.toInt)(coordinate.y.toInt).setColor(cell.cellType.color)
   }

    val posIt = agentPositions.iterator

    while (posIt.hasNext) {
      val next = posIt.next()
      rectangles(next._2.x.toInt)(next._2.y.toInt).setColor(RGBColor.GREEN)
    }

    val buildingsIt = buildingPositions.iterator

    while (buildingsIt.hasNext) {
      val next = buildingsIt.next()
      rectangles(next._2.x.toInt)(next._2.y.toInt).setColor(RGBColor.BLUE)
    }

  })

  def receive: Receive = {
    case RegisterAgentRequest(bot, position) ⇒
      if (position.x >= 0 && position.x < width && position.y >= 0 && position.y < height) {
        if (field(position).cellType != CellType.Forbidden) {
          bot ! BotAgent.Introduction(townHallLocation)
          bot ! BotAgent.PositionChange(position)
          agentPositions = agentPositions + (bot -> position)
        }
      } else {
        bot ! PoisonPill
      }

    case RegisterBuildingRequest(ref, position) ⇒
      field.get(position) match {
        case Some(cell) =>
          if (cell.cellType != CellType.Forbidden) {
            ref ! BotAgent.Introduction(townHallLocation)
            ref ! SimulationAgent.PositionChangeRequest(position)
            buildingPositions = buildingPositions + (ref -> position)
          }
        case None =>  ref ! PoisonPill
      }


    case CellTypeChangeRequest(coordinate, cellType) ⇒
      for(cell <- field.get(coordinate)){
        field += (coordinate -> Cell(cellType))
      }

    case PositionChangeRequest(position) ⇒
      for(cell <- field.get(position)){
        if (cell.cellType != CellType.Forbidden) {
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
      val v = VicinityReponse(agentPositions.filter(t ⇒ Utils.manhattanDistance(pos, t._2) <= dis && t._1 != ref), buildingPositions.filter(t ⇒ Utils.manhattanDistance(pos, t._2) < dis && t._1 != ref))
      sender() ! v

    case CellRequest(ref) ⇒
      val position = agentPositions(ref)
      sender() ! field(position)

    case CellAtIndexRequest(coordinate) ⇒
      for(cell <- field.get(coordinate)){
        sender() ! cell
      }
    case UnregisterRequest ⇒
      agentPositions = agentPositions - sender()

    case VisibleCellsRequest(ref, distance) =>
      val position = agentPositions(ref)
      val cells = field.filter(e => Utils.manhattanDistance(position, e._1) <= distance).toMap
      sender() ! cells

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
  def spawnBot[T <: BotAgent](cls: Class[T], position: Coordinate): Bot = {
    val bot = system.actorOf(Props(cls))
    sim ! RegisterAgentRequest(bot, position)
    Bot(bot)
  }

  def spawnBot[T <: BotAgent](cls: Class[T]): Bot = {
    val bot = system.actorOf(Props(cls))
    sim ! RegisterAgentRequest(bot, randomPosition())
    Bot(bot)
  }

  def spawnBuilding[T <: Building](cls: Class[T], position: Coordinate): ActorRef = {
    val building = system.actorOf(Props(cls))
    sim ! RegisterBuildingRequest(building, position)
    building
  }

  def changeCellType(index: Coordinate, cellType: CellType) = {
    sim ! CellTypeChangeRequest(index, cellType)
  }

  def randomPosition(): Coordinate = Coordinate(Random.nextInt(width), Random.nextInt(height))
}

