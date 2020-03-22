package iteration1

import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class OrderingTest1 extends FlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000, minSize = 50, sizeRange = 20)

  implicit val catGen: Gen[Cat] = for {
    age <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
    name <- Gen.alphaStr
    available <- Gen.oneOf(true, false)
  } yield Cat(age, name, available)

  private val naturalOrdering: Ordering[Cat] = Ordering.fromLessThan((c1, c2) =>
    (c1.age < c2.age) ||
    (c1.age == c2.age) && (c1.name < c2.name) ||
    (c1.age == c2.age) && (c1.name == c2.name) && (c1.available < c2.available)
  )


  "Ascending sort order on all fields" should "be identical to natural ordering" in {
    val providedOrdering: Ordering[Cat] = CatOrdering.of(SortOrder.Asc, SortOrder.Asc, SortOrder.Asc)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(naturalOrdering))
    }
  }

  "Descending sort order on all fields" should "be identical to reversed natural ordering" in {
    val providedOrdering: Ordering[Cat] = CatOrdering.of(SortOrder.Desc, SortOrder.Desc, SortOrder.Desc)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(naturalOrdering.reverse))
    }
  }

  "Any sort order on all fields" should "not change the order of elements" in {
    val providedOrdering: Ordering[Cat] = CatOrdering.of(SortOrder.Keep, SortOrder.Keep, SortOrder.Keep)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats)
    }
  }

  "Ascending sort order only by the \"name\" field" should "be the same as natural ordering by that field" in {
    val providedOrdering: Ordering[Cat] = CatOrdering.of(SortOrder.Keep, SortOrder.Asc, SortOrder.Keep)

    val fieldOrdering: Ordering[Cat] = Ordering.by(_.name)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Descending sort order only by the \"name\" field" should "be the same as reversed natural ordering by that field" in {
    val providedOrdering: Ordering[Cat] = CatOrdering.of(SortOrder.Keep, SortOrder.Desc, SortOrder.Keep)

    val fieldOrdering: Ordering[Cat] = Ordering.by[Cat, String](_.name).reverse

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }
}
