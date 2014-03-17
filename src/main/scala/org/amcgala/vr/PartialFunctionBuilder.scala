package org.amcgala.vr

class PartialFunctionBuilder[A, B] {
  import scala.collection.immutable.Vector

  private var pfsOption: Option[Vector[PartialFunction[A, B]]] = Some(Vector.empty)

  private def mapPfs[C](f: Vector[PartialFunction[A, B]] ⇒ (Option[Vector[PartialFunction[A, B]]], C)): C = {
    pfsOption.fold(throw new IllegalStateException("Already built"))(f) match {
      case (newPfsOption, result) ⇒
        pfsOption = newPfsOption
        result
    }
  }

  def +=(pf: PartialFunction[A, B]): Unit =
    mapPfs { case pfs ⇒ (Some(pfs :+ pf), ()) }

  def result(): PartialFunction[A, B] =
    mapPfs { case pfs ⇒ (None, pfs.foldLeft[PartialFunction[A, B]](Map.empty) { _ orElse _ }) }
}
