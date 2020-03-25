package iteration3

import java.time.{LocalDate, ZoneOffset}

import iteration3.sort_order.SortOrder
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class OrderingTest3 extends FlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2000, minSize = 50, sizeRange = 20)

  implicit val catGen: Gen[Cat] = for {
    age <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
    name <- Gen.alphaStr
    available <- Gen.oneOf(true, false)
    owner <- Gen.option(Gen.alphaStr)
    breed <- Gen.alphaStr
    furColor <- Gen.alphaStr
    eyeColor <- Gen.alphaStr
    registrationId <- Gen.alphaNumStr
    lastHealthCheck <- Gen.option(Gen.calendar.map(_.toInstant.atZone(ZoneOffset.UTC).toLocalDate))
    urgentSell <- Gen.oneOf(true, false)
  } yield Cat(age, name, available, owner, breed, furColor, eyeColor, registrationId, lastHealthCheck, urgentSell)

  val naturalOrdering: Ordering[Cat] =
    Ordering.by[Cat, Int](_.age)
      .orElseBy(_.name)
      .orElseBy(_.available)
      .orElseBy(_.owner)
      .orElseBy(_.breed)
      .orElseBy(_.furColor)
      .orElseBy(_.eyeColor)
      .orElseBy(_.registrationId)
      .orElseBy(_.lastHealthCheck)(Ordering.Option(Ordering.by[LocalDate, Long](_.toEpochDay)))
      .orElseBy(_.urgentSell)

  "Ascending sort order (empty first) on all fields" should "be identical to natural ordering" in {
    val fieldMap: List[(CatField, SortOrder)] = List(
      CatField.Age ->             SortOrder.Asc.emptyFirst,
      CatField.Name ->            SortOrder.Asc.emptyFirst,
      CatField.Available ->       SortOrder.Asc.emptyFirst,
      CatField.Owner ->           SortOrder.Asc.emptyFirst,
      CatField.Breed ->           SortOrder.Asc.emptyFirst,
      CatField.FurColor ->        SortOrder.Asc.emptyFirst,
      CatField.EyeColor ->        SortOrder.Asc.emptyFirst,
      CatField.RegistrationId ->  SortOrder.Asc.emptyFirst,
      CatField.LastHealthCheck -> SortOrder.Asc.emptyFirst,
      CatField.UrgentSell ->      SortOrder.Asc.emptyFirst
    )
    val providedOrdering: Ordering[Cat] = CatOrdering.byFields(fieldMap)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(naturalOrdering))
    }
  }

  "Descending sort order (empty last) on all fields" should "be identical to reversed natural ordering" in {
    val fieldMap: List[(CatField, SortOrder)] = List(
      CatField.Age ->             SortOrder.Desc.emptyLast,
      CatField.Name ->            SortOrder.Desc.emptyLast,
      CatField.Available ->       SortOrder.Desc.emptyLast,
      CatField.Owner ->           SortOrder.Desc.emptyLast,
      CatField.Breed ->           SortOrder.Desc.emptyLast,
      CatField.FurColor ->        SortOrder.Desc.emptyLast,
      CatField.EyeColor ->        SortOrder.Desc.emptyLast,
      CatField.RegistrationId ->  SortOrder.Desc.emptyLast,
      CatField.LastHealthCheck -> SortOrder.Desc.emptyLast,
      CatField.UrgentSell ->      SortOrder.Desc.emptyLast
    )
    val providedOrdering: Ordering[Cat] = CatOrdering.byFields(fieldMap)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(naturalOrdering.reverse))
    }
  }

  "No sort order on all fields" should "not change the order of elements" in {
    val fieldMap: List[(CatField, SortOrder)] = List.empty
    val providedOrdering: Ordering[Cat] = CatOrdering.byFields(fieldMap)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats)
    }
  }

  "Ascending sort order only by the \"name\" field" should "be the same as natural ordering by that field" in {
    val fieldMap: List[(CatField, SortOrder)] = List(
      CatField.Name -> SortOrder.Asc.emptyFirst,
    )
    val providedOrdering: Ordering[Cat] = CatOrdering.byFields(fieldMap)

    val fieldOrdering: Ordering[Cat] = Ordering.by(_.name)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Descending sort order only by the \"name\" field" should "be the same as reversed natural ordering by that field" in {
    val fieldMap: List[(CatField, SortOrder)] = List(
      CatField.Name -> SortOrder.Desc.emptyFirst,
    )
    val providedOrdering: Ordering[Cat] = CatOrdering.byFields(fieldMap)

    val fieldOrdering: Ordering[Cat] = Ordering.by[Cat, String](_.name).reverse

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Ascending sort order (empty first) only by the \"owner\" field" should "be the same as natural ordering by that field" in {
    val fieldMap: List[(CatField, SortOrder)] = List(
      CatField.Owner -> SortOrder.Asc.emptyFirst,
    )
    val providedOrdering: Ordering[Cat] = CatOrdering.byFields(fieldMap)

    val fieldOrdering: Ordering[Cat] = Ordering.by(_.owner)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Descending sort order (empty last) only by the \"owner\" field" should "be the same as reversed natural ordering by that field" in {
    val fieldMap: List[(CatField, SortOrder)] = List(
      CatField.Owner -> SortOrder.Desc.emptyLast,
    )
    val providedOrdering: Ordering[Cat] = CatOrdering.byFields(fieldMap)

    val fieldOrdering: Ordering[Cat] = Ordering.by[Cat, Option[String]](_.owner).reverse

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Ascending sort order (empty last) only by the \"owner\" field" should "be correct" in {
    val fieldMap: List[(CatField, SortOrder)] = List(
      CatField.Owner -> SortOrder.Asc.emptyLast,
    )
    val providedOrdering: Ordering[Cat] = CatOrdering.byFields(fieldMap)
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
    val fieldMap: List[(CatField, SortOrder)] = List(
      CatField.Owner -> SortOrder.Desc.emptyFirst,
    )
    val providedOrdering: Ordering[Cat] = CatOrdering.byFields(fieldMap)

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

  "Customized sort order" should "be correct" in {
    val fieldMap: Seq[(CatField, SortOrder)] = Seq(
      CatField.Owner -> SortOrder.Desc.emptyLast,
      CatField.Name -> SortOrder.Asc.emptyFirst,
      CatField.Available -> SortOrder.Asc.emptyFirst
    )
    val providedOrdering: Ordering[Cat] = CatOrdering.byFields(fieldMap)

    val fieldOrdering: Ordering[Cat] = Ordering.by[Cat, Option[String]](_.owner).reverse
      .orElseBy(_.name)
      .orElseBy(_.available)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }
}
