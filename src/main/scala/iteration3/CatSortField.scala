package iteration3

import java.time.LocalDate

import common.OrderingUtil
import iteration3.sort_order.SortOrder
import iteration3.sort_order.syntax._

import scala.Ordering._
import scala.collection.SortedMap

sealed abstract class CatSortField(val defaultPriority: Int) {
  def toOrdering(sortOrder: SortOrder): Ordering[Cat]
}

object CatSortField {

  implicit val ordering: Ordering[CatSortField] =
    Ordering.by(_.defaultPriority)

  def toDefaultOrdering(fields: Map[CatSortField, SortOrder]): Ordering[Cat] =
    toCustomizedOrdering(SortedMap.from(fields))

  def toCustomizedOrdering(fields: Iterable[(CatSortField, SortOrder)]): Ordering[Cat] =
    if (fields.isEmpty) OrderingUtil.identity[Cat]
    else {
      val (head, headOrder) = fields.head
      val (res, _) = fields.tail.foldLeft[(Ordering[Cat], Set[CatSortField])]((head.toOrdering(headOrder), Set())) {
        case (acc@(_, presentFields), (field, _)) if presentFields.contains(field) =>
          acc

        case ((ordering, presentFields), (field, order)) =>
          (ordering.orElse(field.toOrdering(order)), presentFields + field)
      }
      res
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