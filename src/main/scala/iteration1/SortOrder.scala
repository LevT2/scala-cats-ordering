package iteration1

sealed trait SortOrder

object SortOrder {

  case object Keep extends SortOrder

  case object Asc extends SortOrder

  case object Desc extends SortOrder
}


