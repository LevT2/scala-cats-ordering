package iteration1

object Cat {

  val values: List[Cat] =
    List(
      Cat(8, "Hana", available = true),
      Cat(4, "Momo", available = false),
      Cat(2, "Shiro", available = true),
      Cat(2, "Fuku", available = true),
      Cat(9, "Kuro", available = false),
      Cat(7, "Kuro", available = false)
    )
}

case class Cat(age: Int,
               name: String,
               available: Boolean)
