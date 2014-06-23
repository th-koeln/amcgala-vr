package org.amcgala.vr

import akka.actor.{ Stash, PoisonPill, ActorRef }
import org.amcgala.vr.Headings.Heading
import org.amcgala.vr.SimulationAgent.{CellRequest, VicinityReponse}
import scala.concurrent.{ ExecutionContext, Future }
import akka.pattern.ask
import akka.util.Timeout
import org.amcgala.vr.BotAgent.TurnLeft
import scala.reflect.ClassTag
import org.amcgala.vr.need.Need
import org.amcgala.vr.need.Needs.NeedIDs.NeedID

object BotAgent {

  /**
    * New [[Coordinate]] of this Bot.
    * @param pos the new [[Coordinate]]
    */
  case class PositionChange(pos: Coordinate)

  /**
    * Introduces the [[Simulation]] to this Bot.
    */
  case class Introduction(townHall: Coordinate)

  /**
    * The [[BotAgent]] replies with its current [[Heading]].
    */
  case object HeadingRequest

  case object TurnLeft
  case object TurnRight
  case object MoveForward
  case object MoveBackward

  case class MoveToPosition(position: Coordinate)

  case class ChangeVelocity(vel: Int)

  case object CurrentPositionRequest

  case class ExecuteTask(task: Task)
  case class ExecuteBehavior(behavior: Behavior)

  case class RegisterNeed(need: Need)
  case class RemoveNeed(id: NeedID)

  case class VicinityRequest(distance: Double)

  case class VisibleCellsRequest(distance: Double)
  case object TownHallLocationRequest

  case object TimeRequest
}

/**
  * A Bot is an [[Agent]] with a physical position.
  */
trait BotAgent extends Agent with Stash {

  import concurrent.duration._
  import BotAgent._

  implicit val timeout = Timeout(120.second)
  implicit val ec = ExecutionContext.global
  implicit val me = Bot(self)

  var localPosition: Coordinate = Coordinate(0, 0)
  var currentPosition: Coordinate = Coordinate(0, 0)
  var knownCells = Map[Coordinate, Cell]()

  var heading: Heading = Headings.Up
  var velocity: Int = 1
  var simulation: ActorRef = ActorRef.noSender
  
  private var thLocation = Coordinate(0,0)

  val brain = new Brain(Bot(self))

  registerOnTickAction("update brain", () ⇒ {
    brain.update()
  })

  registerOnTickAction("localMap", () ⇒ {
    if (currentPosition != localPosition) {
      currentPosition = localPosition
      for (cell ← cell()) {
        knownCells = knownCells + (currentPosition -> cell)
      }
    }
  })

  override def postStop(): Unit = {
    simulation ! SimulationAgent.Unregister
  }

  def receive: Receive = {
    case Introduction(townhall) ⇒
      simulation = sender()
      thLocation = townhall
     
    case PositionChange(pos) ⇒
      localPosition = pos
      context.become(defaultHandling)
      unstashAll()
    case _ ⇒ stash()
  }

  protected def defaultHandling = positionHandling orElse tickHandling orElse taskHandling orElse needHandling orElse customReceive

  protected def taskHandling: Receive = {
    case ExecuteBehavior(b) ⇒
      val requester = sender()
      for (r ← brain.executeBehavior(b)) {
        requester ! r
      }
    case ExecuteTask(t) ⇒
      val requester = sender()
      for (r ← brain.executeTask(t)) {
        requester ! r
      }
    case VicinityRequest(distance) =>
      val requester = sender()
      for(v <- vicinity(distance)) requester ! v
    case VisibleCellsRequest(distance) =>
      val requester = sender()
      for(v <- visibleCells(distance)) requester ! v
    case TimeRequest ⇒
      sender() ! currentTime
    case TownHallLocationRequest => sender() ! thLocation
    case CellRequest =>
      val requester = sender()
      for(c <- cell()) requester ! c
  }

  protected def needHandling: Receive = {
    case RegisterNeed(need) ⇒ brain.registerNeed(need)
    case RemoveNeed(id)     ⇒ brain.removeNeed(id)
  }

  protected def positionHandling: Receive = {
    case PositionChange(pos) ⇒
      localPosition = pos
    case HeadingRequest         ⇒ sender() ! heading
    case TurnLeft               ⇒
      turnLeft()
    case TurnRight              ⇒
      turnRight()
    case MoveBackward           ⇒  moveBackward()
    case MoveForward            ⇒  moveForward()
    case MoveToPosition(pos)    ⇒  moveToPosition(pos)
    case ChangeVelocity(vel)    ⇒  velocity = vel
    case CurrentPositionRequest ⇒ sender() ! localPosition
  }

  /**
    * The Bot turns left.
    * @return the new [[Heading]] of the Bot after turning left
    */
  def turnLeft(): Heading = {
    heading match {
      case Headings.Up        ⇒ heading = Headings.UpLeft
      case Headings.UpLeft    ⇒ heading = Headings.Left
      case Headings.Left      ⇒ heading = Headings.DownLeft
      case Headings.DownLeft  ⇒ heading = Headings.Down
      case Headings.Down      ⇒ heading = Headings.DownRight
      case Headings.DownRight ⇒ heading = Headings.Right
      case Headings.Right     ⇒ heading = Headings.UpRight
      case Headings.UpRight   ⇒ heading = Headings.Up
    }
    heading
  }

  /**
    * The Bot turns right.
    * @return the new [[Heading]] of the Bot after turning right.
    */
  def turnRight(): Heading = {
    heading match {
      case Headings.Up        ⇒ heading = Headings.UpRight
      case Headings.UpLeft    ⇒ heading = Headings.Up
      case Headings.Left      ⇒ heading = Headings.UpLeft
      case Headings.DownLeft  ⇒ heading = Headings.Left
      case Headings.Down      ⇒ heading = Headings.DownLeft
      case Headings.DownRight ⇒ heading = Headings.Down
      case Headings.Right     ⇒ heading = Headings.DownRight
      case Headings.UpRight   ⇒ heading = Headings.Right
    }
    heading
  }

  /**
    * The Bot looks into the new direction.
    * @param newHeading the new [[Heading]]
    */
  def turnUntil(newHeading: Heading): Unit = heading = newHeading

  /**
    * The Bot moves one step forward.
    */
  def moveForward(): Unit = {
    for (pos ← position()) {
      simulation ! SimulationAgent.PositionChange(Coordinate(pos.x + heading.x * velocity, pos.y + heading.y * velocity))
    }
  }

  /**
    * The Bot moves one step backward.
    */
  def moveBackward(): Unit = {
    for (pos ← position()) {
      simulation ! SimulationAgent.PositionChange(Coordinate(pos.x - heading.x * velocity, pos.y - heading.y * velocity))
    }
  }

  def moveToPosition(pos: Coordinate) = simulation ! SimulationAgent.PositionChange(pos)

  /**
    * Gets the current [[Coordinate]] of this Bot from the [[Simulation]].
    * @return the current position
    */
  def position(): Future[Coordinate] = (simulation ? SimulationAgent.PositionRequest(self)).mapTo[Coordinate]

  /**
    * Gets the current cell from the [[Simulation]].
    * @return the current [[Cell]]
    */
  def cell(): Future[Cell] = (simulation ? SimulationAgent.CellRequest(self)).mapTo[Cell]

  /**
    * Gets the cell at an arbitrary location on the map.
    * @param index the index
    * @return
    */
  def cell(index: Coordinate): Future[Cell] = (simulation ? SimulationAgent.CellAtIndexRequest(index)).mapTo[Cell]

  /**
    * Gets all [[BotAgent]]s in the vicinity of this Bot.
    * @param distance the radius of the vicinity
    * @return all [[ActorRef]]s and their positions in the [[Simulation]]
    */
  def vicinity(distance: Double): Future[VicinityReponse] = (simulation ? SimulationAgent.VicinityRequest(self, distance)).mapTo[VicinityReponse]

  def visibleCells(distance: Double): Future[Map[Coordinate, Cell]] = (simulation ? SimulationAgent.VisibleCellsRequest(self, distance)).mapTo[Map[Coordinate, Cell]]

  /**
    * Requests the current [[Heading]] of a Bot.
    * @param ref the [[ActorRef]] of the Bot
    * @return the current [[Heading]]
    */
  def requestHeading(ref: ActorRef): Future[Heading] = (ref ? BotAgent.HeadingRequest).mapTo[Heading]

  /**
    * Changes the velocity of the Bot.
    * @param change the velocity change
    */
  def changeVelocity(change: Int): Unit = velocity += change

  def worldSize: (Int, Int) = (200, 200)

  def registerNeed(need: Need) = brain.registerNeed(need)

  def registerJob(job: Behavior) = brain.registerJob(job)
  
  def townHallLocation: Coordinate = thLocation
}

case class Bot(ref: ActorRef) {
  import BotAgent._
  import Agent._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import akka.pattern.ask

  private implicit val timeout = Timeout(120.seconds)

  /**
    * Left turn (defaults to a left turn of 45°).
    */
  def turnLeft() = ref ! TurnLeft

  /**
    * Right turn (defaults to a right turn of 45°).
    */
  def turnRight() = ref ! TurnRight

  /**
    * The bot moves one step forward.
    */
  def moveForward() = ref ! MoveForward

  /**
    * The bot moves one step backward.
    */
  def moveBackward() = ref ! MoveBackward

  /**
    * The bot jumps to a new position.
    * @param pos the new position
    */
  def moveToPosition(pos: Coordinate) = ref ! MoveToPosition(pos)

  /**
    * The current position.
    * @return
    */
  def position() = (ref ? CurrentPositionRequest).mapTo[Coordinate]

  /**
    * The bot moves with a new velocity.
    * @param vel the new velocity
    */
  def changeVelocity(vel: Int) = ref ! ChangeVelocity(vel)

  /**
    * Registers a new action that is executed on every simulation tick.
    * @param handle the name of this action
    * @param action the function to be executed
    */
  def registerOnTickAction(handle: String, action: () ⇒ Unit) = ref ! RegisterOnTickAction(handle, action)

  /**
    * Removes an action.
    * @param handle the handle of the action
    */
  def removeOnTickAction(handle: String) = ref ! RemoveOnTickAction(handle)

  /**
    * Executes a [[Task]].
    * @param task the [[Task]] to be executed
    * @param tag class evidence
    * @return the result of the task
    */
  def executeTask(task: Task)(implicit tag: ClassTag[task.Return]): Future[task.Return] = (ref ? ExecuteTask(task)).mapTo[task.Return]

  /**
    * Executes a [[Behavior]]
    * @param behavior the [[Behavior]] to be executed
    * @param tag
    * @return
    */
  def executeBehavior(behavior: Behavior)(implicit tag: ClassTag[behavior.Return]): Future[behavior.Return] = {
    (ref ? ExecuteBehavior(behavior)).mapTo[behavior.Return]
  }

  /**
    * Adds a new need to the Bot's [[org.amcgala.vr.need.NeedManager]]
    * @param need the new need
    */
  def registerNeed(need: Need) = ref ! RegisterNeed(need)

  /**
    * Removes a need from the [[org.amcgala.vr.need.NeedManager]].
    * @param id the [[NeedID]]
    */
  def removeNeed(id: NeedID) = ref ! RemoveNeed(id)

  /**
    * Returns the current [[Time]] in the simulation.
    * @return
    */
  def currentTime = (ref ? TimeRequest).mapTo[Time]

  def vicinity(distance: Double): Future[VicinityReponse] = (ref ? VicinityRequest(distance)).mapTo[VicinityReponse]

  def townHall = (ref ? TownHallLocationRequest).mapTo[Coordinate]

  def currentCell: Future[Cell] = (ref ? CellRequest).mapTo[Cell]

  def visibleCells(distance: Double): Future[Map[Coordinate, Cell]] = (ref ? VisibleCellsRequest(distance)).mapTo[Map[Coordinate, Cell]]
}
