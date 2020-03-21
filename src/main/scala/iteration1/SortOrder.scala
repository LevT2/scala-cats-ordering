package iteration1

sealed trait SortOrder

object SortOrder {

  case object Any extends SortOrder
  case object Asc extends SortOrder
  case object Desc extends SortOrder
}


