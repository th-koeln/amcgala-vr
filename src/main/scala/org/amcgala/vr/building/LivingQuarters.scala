package org.amcgala.vr.building

object LivingQuarters {

  case object RegisterBot

}

class LivingQuarters extends Building {

  import LivingQuarters._

  val taskHandling: Receive = ???
}
