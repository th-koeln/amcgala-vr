package org.amcgala

import akka.actor.ActorSystem

package object vr {
  import akka.actor.Actor.Receive
  import akka.actor.{ PoisonPill, ActorRef, ActorSystem, Props, Actor }

  implicit val system = ActorSystem("vr-system")

  object Headings {
    /**
      * Die Blickrichtung des Agenten in relativen Koordinaten. Da es sich um ein direktes Feld handelt, wird einfach
      * die Richtung zum nächsten Nachbarn angegeben, in die der Agent gerade blickt.
      */
    sealed trait Heading {
      val x: Double
      val y: Double
    }

    case object Up extends Heading {
      val x: Double = 0
      val y: Double = -1
    }

    case object Down extends Heading {
      val x: Double = 0
      val y: Double = 1
    }

    case object Left extends Heading {
      val x: Double = -1
      val y: Double = 0
    }

    case object Right extends Heading {
      val x: Double = 1
      val y: Double = 0
    }

    case object UpLeft extends Heading {
      val x: Double = -1
      val y: Double = -1
    }

    case object UpRight extends Heading {
      val x: Double = 1
      val y: Double = -1
    }

    case object DownLeft extends Heading {
      val x: Double = -1
      val y: Double = 1
    }
    case object DownRight extends Heading {
      val x: Double = 1
      val y: Double = 1
    }
  }

  object CellTypes {
    sealed trait CellType {
      val movementCost: Double
    }

    case object Floor extends CellType {
      val movementCost: Double = 0.1
    }

    case object Wall extends CellType {
      val movementCost: Double = 1000
    }

    case object Forbidden extends CellType {
      val movementCost: Double = Double.PositiveInfinity
    }
  }

  object Utils {
    def distance(positionA: Position, positionB: Position): Double = scala.math.sqrt(scala.math.pow(positionA.x - positionB.x, 2) + scala.math.pow(positionA.y - positionB.y, 2))

  }

}
