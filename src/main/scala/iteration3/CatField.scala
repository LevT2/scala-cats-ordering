package iteration3

import java.time.LocalDate

import iteration3.sort_order.SortOrder
import iteration3.sort_order.syntax._

import scala.Ordering._

sealed trait CatField {
  def toOrdering(sortOrder: SortOrder): Ordering[Cat]
}

object CatField {

  case object Age extends CatField {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.Int).on(_.age)
  }

  case object Name extends CatField {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.String).on(_.name)
  }

  case object Available extends CatField {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.Boolean).on(_.available)
  }

  case object Owner extends CatField {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder.optional(Ordering.String).on(_.owner)
  }

  case object Breed extends CatField {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.String).on(_.breed)
  }

  case object FurColor extends CatField {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.String).on(_.furColor)
  }

  case object EyeColor extends CatField {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.String).on(_.eyeColor)
  }

  case object RegistrationId extends CatField {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.String).on(_.registrationId)
  }

  case object LastHealthCheck extends CatField {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder.optional(Ordering.by[LocalDate, Long](_.toEpochDay)).on(_.lastHealthCheck)
  }

  case object UrgentSell extends CatField {
    override def toOrdering(sortOrder: SortOrder): Ordering[Cat] =
      sortOrder(Ordering.Boolean).on(_.urgentSell)
  }
}