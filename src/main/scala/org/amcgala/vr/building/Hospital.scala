package org.amcgala.vr.building

object Hospital {

  case object TreatmentRequest

}

class Hospital extends Building {

  import Hospital._

  def taskHandling: Receive = {
    case TreatmentRequest ⇒
    // TODO jemand muss geheilt werden.
    // TODO ist er im Hospital oder schreit er einmal quer über die Map?
  }
}
