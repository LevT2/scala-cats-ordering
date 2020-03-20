package iteration2.sort_order

sealed trait SortOrder

object SortOrder {

  case object Any extends SortOrder

  object Asc {
    def nullsFirst: Asc = Asc(nullsFirst = true)
    def nullsLast: Asc = Asc(nullsFirst = false)
  }

  case class Asc(nullsFirst: Boolean) extends SortOrder

  object Desc {
    def nullsFirst: Desc = Desc(nullsFirst = true)
    def nullsLast: Desc = Desc(nullsFirst = false)
  }

  case class Desc(nullsFirst: Boolean) extends SortOrder
}



