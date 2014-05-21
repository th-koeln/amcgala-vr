package org.amcgala

import akka.actor.ActorSystem
import org.amcgala.vr.Simulation

package object vr {
  import akka.actor.Actor.Receive
  import akka.actor.{ PoisonPill, ActorRef, ActorSystem, Props, Actor }

  implicit val system = ActorSystem("vr-system")

  /**
    * Possible headings of a [[BotAgent]].
    */
  object Headings {
    /**
      * The Heading of a [[BotAgent]].
      */
    sealed trait Heading {
      val x: Int
      val y: Int
    }

    case object Up extends Heading {
      val x: Int = 0
      val y: Int = -1
    }

    case object Down extends Heading {
      val x: Int = 0
      val y: Int = 1
    }

    case object Left extends Heading {
      val x: Int = -1
      val y: Int = 0
    }

    case object Right extends Heading {
      val x: Int = 1
      val y: Int = 0
    }

    case object UpLeft extends Heading {
      val x: Int = -1
      val y: Int = -1
    }

    case object UpRight extends Heading {
      val x: Int = 1
      val y: Int = -1
    }

    case object DownLeft extends Heading {
      val x: Int = -1
      val y: Int = 1
    }
    case object DownRight extends Heading {
      val x: Int = 1
      val y: Int = 1
    }
  }

  object Utils {
    def distance(positionA: Position, positionB: Position): Double = scala.math.sqrt(scala.math.pow(positionA.x - positionB.x, 2) + scala.math.pow(positionA.y - positionB.y, 2))
  }

}

sealed trait CellType {
  val movementCost: Double
  val color: RGBColor
}

/**
  * Possible [[CellType]]s of a [[org.amcgala.vr.Cell]] in the [[Simulation]]
  */
object CellType {

  case object Road extends CellType {
    val movementCost: Double = 0.6
    val color: RGBColor = new RGBColor(0.3f, 0.3f, 0.3f)
  }

  case object Floor extends CellType {
    val movementCost: Double = 1
    val color: RGBColor = RGBColor.WHITE
  }

  case object Wall extends CellType {
    val movementCost: Double = 1000
    val color: RGBColor = new RGBColor(0.4f, 0f, 0.2f)
  }

  case object Grass extends CellType {
    val movementCost: Double = 1.2
    val color: RGBColor = new RGBColor(0.2f, 0.2f, 0.3f)
  }

  case object Forbidden extends CellType {
    val movementCost: Double = Double.PositiveInfinity
    val color: RGBColor = RGBColor.RED
  }

}