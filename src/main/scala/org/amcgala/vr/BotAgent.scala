package org.amcgala.vr

import akka.actor.{ Stash, PoisonPill, ActorRef }
import org.amcgala.vr.Headings.Heading
import scala.concurrent.{ ExecutionContext, Future }
import akka.pattern.ask
import akka.util.Timeout
import org.amcgala.vr.BotAgent.TurnLeft
import scala.reflect.ClassTag
import org.amcgala.vr.need.Need
import org.amcgala.vr.need.Needs.NeedIDs.NeedID

object BotAgent {

  /**
    * New [[Position]] of this Bot.
    * @param pos the new [[Position]]
    */
  case class PositionChange(pos: Position)

  /**
    * Introduces the [[Simulation]] to this Bot.
    */
  case object Introduction

  /**
    * The [[BotAgent]] replies with its current [[Heading]].
    */
  case object HeadingRequest

  case object TurnLeft
  case object TurnRight
  case object MoveForward
  case object MoveBackward

  case class MoveToPosition(position: Position)

  case class ChangeVelocity(vel: Int)

  case object CurrentPositionRequest

  case class ExecuteTask(task: Task)
  case class ExecuteBehavior(behavior: Behavior)

  case class RegisterNeed(need: Need)
  case class RemoveNeed(id: NeedID)

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

  var localPosition: Position = Position(0, 0)
  var currentPosition: Position = Position(0, 0)
  var knownCells = Map[Position, Cell]()

  var heading: Heading = Headings.Up
  var velocity: Int = 1
  var simulation: ActorRef = ActorRef.noSender

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
    case Introduction ⇒
      simulation = sender()
    case PositionChange(pos) ⇒
      localPosition = pos
      context.become(positionHandling orElse tickHandling orElse taskHandling orElse needHandling)
      unstashAll()
    case _ ⇒ stash()
  }

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
    case TimeRequest ⇒
      sender() ! currentTime
  }

  protected def needHandling: Receive = {
    case RegisterNeed(need) ⇒ brain.registerNeed(need)
    case RemoveNeed(id)     ⇒ brain.removeNeed(id)
  }

  protected def positionHandling: Receive = {
    case PositionChange(pos) ⇒
      localPosition = pos
    case HeadingRequest         ⇒ sender() ! heading
    case TurnLeft               ⇒ turnLeft()
    case TurnRight              ⇒ turnRight()
    case MoveBackward           ⇒ moveBackward()
    case MoveForward            ⇒ moveForward()
    case MoveToPosition(pos)    ⇒ moveToPosition(pos)
    case ChangeVelocity(vel)    ⇒ velocity = vel
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
      simulation ! SimulationAgent.PositionChange(Position(pos.x + heading.x * velocity, pos.y + heading.y * velocity))
    }
  }

  /**
    * The Bot moves one step backward.
    */
  def moveBackward(): Unit = {
    for (pos ← position()) {
      simulation ! SimulationAgent.PositionChange(Position(pos.x - heading.x * velocity, pos.y - heading.y * velocity))
    }
  }

  def moveToPosition(pos: Position) = simulation ! SimulationAgent.PositionChange(pos)

  /**
    * Gets the current [[Position]] of this Bot from the [[Simulation]].
    * @return the current position
    */
  def position(): Future[Position] = (simulation ? SimulationAgent.PositionRequest(self)).mapTo[Position]

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
  def cell(index: Position): Future[Cell] = (simulation ? SimulationAgent.CellAtIndexRequest(index)).mapTo[Cell]

  /**
    * Gets all [[BotAgent]]s in the vicinity of this Bot.
    * @param distance the radius of the vicinity
    * @return all [[ActorRef]]s and their positions in the [[Simulation]]
    */
  def vicinity(distance: Int): Future[Map[ActorRef, Position]] = (simulation ? SimulationAgent.VicinityRequest(self, distance)).mapTo[Map[ActorRef, Position]]

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
  def moveToPosition(pos: Position) = ref ! MoveToPosition(pos)

  /**
    * The current position.
    * @return
    */
  def position() = (ref ? CurrentPositionRequest).mapTo[Position]

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
}
