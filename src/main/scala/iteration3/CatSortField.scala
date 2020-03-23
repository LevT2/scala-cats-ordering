package iteration3

import java.time.LocalDate

import common.OrderingUtil
import iteration3.sort_order.SortOrder
import iteration3.sort_order.syntax._
import Ordering._

import scala.collection.SortedMap

sealed abstract class CatSortField(val defaultPriority: Int) {
  def toOrdering(sortOrder: SortOrder): Ordering[Cat]
}

object CatSortField {

  implicit val ordering: Ordering[CatSortField] =
    Ordering.by(_.defaultPriority)

  def toDefaultOrdering(fields: Map[CatSortField, SortOrder]): Ordering[Cat] = {
    toCustomizedOrdering(SortedMap.from(fields))
  }

  def toCustomizedOrdering(fields: SortedMap[CatSortField, SortOrder]): Ordering[Cat] = {
    if (fields.isEmpty) OrderingUtil.identity[Cat]
    else {
      val (headField, headOrder) = fields.head
      fields.tail.foldLeft(headField.toOrdering(headOrder)) {
        case (ordering, (field, order)) => ordering.orElse(field.toOrdering(order))
      }
    }
  }

  case object Age extends CatSortField(1) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.Int).on(_.age)
  }

  case object Name extends CatSortField(2) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.String).on(_.name)
  }

  case object Available extends CatSortField(3) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.Boolean).on(_.available)
  }

  case object Owner extends CatSortField(4) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder.optional(Ordering.String).on(_.owner)
  }

  case object Breed extends CatSortField(5) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.String).on(_.breed)
  }

  case object FurColor extends CatSortField(6) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.String).on(_.furColor)
  }

  case object EyeColor extends CatSortField(7) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.String).on(_.eyeColor)
  }

  case object RegistrationId extends CatSortField(8) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.String).on(_.registrationId)
  }

  case object LastHealthCheck extends CatSortField(9) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder.optional(Ordering.by[LocalDate, Long](_.toEpochDay)).on(_.lastHealthCheck)
  }

  case object UrgentSell extends CatSortField(10) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.Boolean).on(_.urgentSell)
  }
}