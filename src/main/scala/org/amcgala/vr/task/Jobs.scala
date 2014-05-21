package org.amcgala.vr.task

import org.amcgala.vr.{ Bot, Behavior }
import scala.concurrent.Future

class SicknessSpreader(val bot: Bot) extends Behavior {
  type Return = Unit.type

  def start(): Future[Return] = ???
}

class BobBuilder(val bot: Bot) extends Behavior {
  type Return = this.type

  def start(): Future[Return] = ???
}

class HouseMD(val bot: Bot) extends Behavior {
  type Return = this.type

  def start(): Future[Return] = ???
}