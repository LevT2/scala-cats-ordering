package iteration3

import common.OrderingUtil
import iteration3.sort_order.SortOrder

object CatOrdering {

  def byFields(fields: Seq[(CatField, SortOrder)]): Ordering[Cat] =
    if (fields.isEmpty) OrderingUtil.identity[Cat]
    else {
      val (head, headOrder) = fields.head
      val (res, _) = fields.tail.foldLeft[(Ordering[Cat], Set[CatField])]((head.toOrdering(headOrder), Set())) {
        case (acc@(_, presentFields), (field, _)) if presentFields.contains(field) =>
          acc

        case ((ordering, presentFields), (field, order)) =>
          (ordering.orElse(field.toOrdering(order)), presentFields + field)
      }
      res
    }
}
