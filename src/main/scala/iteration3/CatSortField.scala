package iteration3

import java.time.LocalDate
import java.util.Comparator

import common.OrderingUtil
import iteration3.sort_order.SortOrder
import iteration3.sort_order.syntax._
import Ordering._

import scala.collection.SortedMap

sealed abstract class CatSortField(val priority: Int) {
  def toOrdering(sortOrder: SortOrder): Ordering[Cat]
}

object CatSortField {

  implicit val ordering: Ordering[CatSortField] =
    Ordering.by(_.priority)

  def toOrdering(fields: Map[CatSortField, SortOrder]): Ordering[Cat] = {
    val comparator = SortedMap.from(fields).foldLeft[Comparator[Cat]](OrderingUtil.identity[Cat]) {
      case (ordering, (field, order)) => ordering.thenComparing(field.toOrdering(order))
    }
    comparatorToOrdering(comparator)
  }

  case object Age extends CatSortField(1) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      Ordering.by[Cat, Int](_.age)(sortOrder(Ordering.Int))
  }

  case object Name extends CatSortField(2) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      Ordering.by[Cat, String](_.name)(sortOrder(Ordering.String))
  }

  case object Available extends CatSortField(3) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      Ordering.by[Cat, Boolean](_.available)(sortOrder(Ordering.Boolean))
  }

  case object Owner extends CatSortField(4) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      Ordering.by[Cat, Option[String]](_.owner)(sortOrder.optional(Ordering.String))
  }

  case object Breed extends CatSortField(5) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      Ordering.by[Cat, String](_.breed)(sortOrder(Ordering.String))
  }

  case object FurColor extends CatSortField(6) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      Ordering.by[Cat, String](_.furColor)(sortOrder(Ordering.String))
  }

  case object EyeColor extends CatSortField(7) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      Ordering.by[Cat, String](_.eyeColor)(sortOrder(Ordering.String))
  }

  case object RegistrationId extends CatSortField(8) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      Ordering.by[Cat, String](_.registrationId)(sortOrder(Ordering.String))
  }

  case object LastHealthCheck extends CatSortField(9) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      Ordering.by[Cat, Option[LocalDate]](_.lastHealthCheck)(sortOrder.optional(Ordering.by(_.toEpochDay)))
  }

  case object UrgentSell extends CatSortField(10) {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      Ordering.by[Cat, Boolean](_.urgentSell)(sortOrder(Ordering.Boolean))
  }
}