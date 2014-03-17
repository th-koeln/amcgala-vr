package org.amcgala.vr

import akka.actor.{ PoisonPill, ActorRef }
import org.amcgala.vr.Headings.Heading
import scala.concurrent.{ ExecutionContext, Future }
import akka.pattern.ask
import akka.util.Timeout
import scala.util.Random

object Bot {

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
    * The [[Bot]] replies with its current [[Heading]].
    */
  case object HeadingRequest

}

/**
  * A Bot is an [[Agent]] with a physical position.
  */
trait Bot extends Agent {

  import concurrent.duration._
  import Bot._

  implicit val timeout = Timeout(1.second)
  implicit val ec = ExecutionContext.global

  var localPosition: Position = Position(0, 0)
  var heading: Heading = Headings.Up
  var velocity: Int = 1
  var simulation: ActorRef = ActorRef.noSender
  var customReceive = new PartialFunctionBuilder[Any, Unit]()

  override def postStop(): Unit = {
    simulation ! SimulationAgent.Unregister
  }

  def receive: Receive = {
    case Introduction ⇒
      simulation = sender()
    case PositionChange(pos) ⇒
      localPosition = pos
      context.become(positionHandling orElse tickHandling orElse custom)
  }

  protected def positionHandling: Receive = {
    case PositionChange(pos) ⇒
      localPosition = pos
    case HeadingRequest ⇒ sender() ! heading
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
      simulation ! SimulationAgent.ChangePosition(Position(pos.x + heading.x * velocity, pos.y + heading.y * velocity))
    }
  }

  /**
    * The Bot moves one step backward.
    */
  def moveBackward(): Unit = {
    for (pos ← position()) {
      simulation ! SimulationAgent.ChangePosition(Position(pos.x - heading.x * velocity, pos.y - heading.y * velocity))
    }
  }

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
    * Gets the current [[Agent]]s in the vicinity of this Bot.
    * @param distance the radius of the vicinity
    * @return all [[ActorRef]]s and their positions in the [[Simulation]]
    */
  def vicinity(distance: Int): Future[Map[ActorRef, Position]] = (simulation ? SimulationAgent.VicinityRequest(self, distance)).mapTo[Map[ActorRef, Position]]

  /**
    * Requests the current [[Heading]] of a Bot.
    * @param ref the [[ActorRef]] of the Bot
    * @return the current [[Heading]]
    */
  def requestHeading(ref: ActorRef): Future[Heading] = (ref ? Bot.HeadingRequest).mapTo[Heading]

  /**
    * Changes the velocity of the Bot.
    * @param change the velocity change
    */
  def changeVelocity(change: Int): Unit = velocity += change

  private def custom: Receive = customReceive.result()
}

/**
  * Marker trait for all things a [[Bot]] is capable of doing.
  */
trait BotCapability

/**
  * A simple Hunger definition for a [[Bot]]. It depends on the [[Health]] [[BotCapability]].
  */
trait Hunger extends BotCapability {
  bot: Bot with Health ⇒

  var hunger: Int = 0
  var ticks: Int = 0

  registerOnTickAction(() ⇒ {
    ticks = (ticks + 1) % 1

    if (ticks == 0) {
      hunger += 1
    }

    if (hunger > 100 && ticks == 0) {
      health -= 1
    }
  })
}

/**
  * Adds health to the [[Bot]]. If the health of a Bot is lower than 0, the Bot dies and is removed from the [[Simulation]].
  */
trait Health extends BotCapability {
  bot: Bot ⇒

  var health: Int = 100

  registerOnTickAction(() ⇒ {
    if (health < 0) {
      self ! PoisonPill
    }
  })
}

/**
 * Lets the [[Bot]] walk randomly.
 */
trait RandomWalk extends BotCapability {
  bot: Bot ⇒

  registerOnTickAction(() ⇒ {
    val turns = Random.nextInt(3)
    val left = Random.nextBoolean()

    if (left) {
      for (i ← 0 to turns) {
        turnLeft()
      }
    } else {
      for (i ← 0 to turns) {
        turnRight()
      }
    }
    moveForward()
  })
}

