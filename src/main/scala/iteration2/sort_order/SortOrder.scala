package iteration2.sort_order

sealed trait SortOrder

object SortOrder {

  case object Any extends SortOrder

  object Asc {
    def emptyFirst: Asc = Asc(emptyFirst = true)
    def emptyLast: Asc = Asc(emptyFirst = false)
  }

  case class Asc(emptyFirst: Boolean) extends SortOrder

  object Desc {
    def emptyFirst: Desc = Desc(emptyFirst = true)
    def emptyLast: Desc = Desc(emptyFirst = false)
  }

  case class Desc(emptyFirst: Boolean) extends SortOrder
}



