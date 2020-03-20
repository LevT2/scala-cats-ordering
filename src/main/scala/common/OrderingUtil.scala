package common

object OrderingUtil {

  def identity[A]: Ordering[A] = Ordering.by(_ => 0)
}
