package iteration2

import iteration2.sort_order.SortOrder
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class OrderingTest2 extends FlatSpec with Matchers with ScalaCheckDrivenPropertyChecks  {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2000, minSize = 50, sizeRange = 20)

  implicit val catGen: Gen[Cat] = for {
    age <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
    name <- Gen.alphaStr
    available <- Gen.oneOf(true, false)
    owner <- Gen.option(Gen.alphaStr)
  } yield Cat(age, name, available, owner)

  val naturalOrdering: Ordering[Cat] = Ordering.fromLessThan((c1, c2) =>
    (c1.age < c2.age) ||
    (c1.age == c2.age) && (c1.name < c2.name) ||
    (c1.age == c2.age) && (c1.name == c2.name) && (c1.available < c2.available) ||
    (c1.age == c2.age) && (c1.name == c2.name) && (c1.available == c2.available) && Ordering[Option[String]].lt(c1.owner, c2.owner)
  )

  "Ascending sort order (empty first) on all fields" should "be identical to natural ordering" in {
    val providedOrdering: Ordering[Cat] = CatOrdering.toOrdering(SortOrder.Asc.emptyFirst, SortOrder.Asc.emptyFirst, SortOrder.Asc.emptyFirst, SortOrder.Asc.emptyFirst)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(naturalOrdering))
    }
  }

  "Descending sort order (empty last) on all fields" should "be identical to reversed natural ordering" in {
    val providedOrdering: Ordering[Cat] = CatOrdering.toOrdering(SortOrder.Desc.emptyLast, SortOrder.Desc.emptyLast, SortOrder.Desc.emptyLast, SortOrder.Desc.emptyLast)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(naturalOrdering.reverse))
    }
  }

  "Keeping sort order on all fields" should "not change the order of elements" in {
    val providedOrdering: Ordering[Cat] = CatOrdering.toOrdering(SortOrder.Keep, SortOrder.Keep, SortOrder.Keep, SortOrder.Keep)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats)
    }
  }

  "Ascending sort order only by the \"name\" field" should "be the same as natural ordering by that field" in {
    val providedOrdering: Ordering[Cat] = CatOrdering.toOrdering(SortOrder.Keep, SortOrder.Asc.emptyFirst, SortOrder.Keep, SortOrder.Keep)

    val fieldOrdering: Ordering[Cat] = Ordering.by(_.name)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Descending sort order only by the \"name\" field" should "be the same as reversed natural ordering by that field" in {
    val providedOrdering: Ordering[Cat] = CatOrdering.toOrdering(SortOrder.Keep, SortOrder.Desc.emptyFirst, SortOrder.Keep, SortOrder.Keep)

    val fieldOrdering: Ordering[Cat] = Ordering.by[Cat, String](_.name).reverse

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Ascending sort order (empty first) only by the \"owner\" field" should "be the same as natural ordering by that field" in {
    val providedOrdering: Ordering[Cat] = CatOrdering.toOrdering(SortOrder.Keep, SortOrder.Keep, SortOrder.Keep, SortOrder.Asc.emptyFirst)

    val fieldOrdering: Ordering[Cat] = Ordering.by(_.owner)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Descending sort order (empty last) only by the \"owner\" field" should "be the same as reversed natural ordering by that field" in {
    val providedOrdering: Ordering[Cat] = CatOrdering.toOrdering(SortOrder.Keep, SortOrder.Keep, SortOrder.Keep, SortOrder.Desc.emptyLast)

    val fieldOrdering: Ordering[Cat] = Ordering.by[Cat, Option[String]](_.owner).reverse

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Ascending sort order (empty last) only by the \"owner\" field" should "be correct" in {
    val providedOrdering: Ordering[Cat] = CatOrdering.toOrdering(SortOrder.Keep, SortOrder.Keep, SortOrder.Keep, SortOrder.Asc.emptyLast)

    val fieldOrdering: Ordering[Cat] = Ordering.fromLessThan[Option[String]] {
      case (None, None) => false
      case (None, _) => false
      case (_, None) => true
      case (Some(x), Some(y)) => x < y
    }.on(_.owner)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Descending sort order (empty first) only by the \"owner\" field" should "be correct" in {
    val providedOrdering: Ordering[Cat] = CatOrdering.toOrdering(SortOrder.Keep, SortOrder.Keep, SortOrder.Keep, SortOrder.Desc.emptyFirst)

    val fieldOrdering: Ordering[Cat] = Ordering.fromLessThan[Option[String]] {
      case (None, None) => false
      case (None, _) => true
      case (_, None) => false
      case (Some(x), Some(y)) => x > y
    }.on(_.owner)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }
}
