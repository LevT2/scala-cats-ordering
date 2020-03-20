package iteration2

object Cat {

  val values: List[Cat] =
    List(
      Cat(8, "Hana", available = true, Some("Hattori Hanzo")),
      Cat(4, "Momo", available = false, None),
      Cat(2, "Shiro", available = true, None),
      Cat(2, "Fuku", available = true, Some("Hattori Hanzo")),
      Cat(9, "Kuro", available = false, Some("Shinzo Abe")),
      Cat(7, "Kuro", available = false, None)
    )
}

case class Cat(age: Int,
               name: String,
               available: Boolean,
               owner: Option[String])
