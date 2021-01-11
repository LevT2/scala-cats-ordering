val seq = sequenceOf(
    mapOf("Tarski" to 10, "Gödel" to 20), mapOf("Church" to 100, "Gentzen" to 50),
    mapOf("Tarski" to 50), mapOf("Banach" to 15, "Gentzen" to 35)
)

fun main() = payments(seq).also(::println)

fun <Person, Amount> payments (data: Sequence<Map<Person, Amount>>): Map<Person, List<Amount>> =
    data.flatMap { x -> x.entries }
        .groupBy { it.component1() }
        .mapValues { it.component2().map { it.value } }