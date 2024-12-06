import collection.mutable.{HashMap, MultiMap, Set}

val day4input = os
  .read(os.pwd / "inputs" / "day5.txt")
  .split("\n\n") /*>  : Array[String] = [Ljava.lang.String;@735321e7  */

val rules = day4input.head
  .split(
    "\n"
  )
  .map { s =>
    s.split("""\|""").toList
  }
  .toList /*>  : List[List[String]] = List(List(83, 24), List(88, 31), List(88, 42), List(34, 24), List(34, 74), List(34, 89),…  */
val produce = day4input.last
  .split(
    "\n"
  )
  .map(_.split(",").map(_.toInt).toList)
  .toList /*>  : List[List[Int]] = List(List(46, 98, 96, 88, 42), List(83, 87, 21, 23, 12, 85, 27, 89, 63, 28, 97, 13, 81), Li…  */

val rulesMap =
  new HashMap[Int, Set[Int]]
    with MultiMap[
      Int,
      Int
    ] /*>  : HashMap[Int, Set[Int]] & MultiMap[Int, Int] = HashMap(11 -> HashSet(75, 12, 13, 78, 81, 82, 21, 85, 86, 23, 87, 2…  */ /*>  : HashMap[Int, Set[I…  */ /*>  : HashMap[Int, Set[I…  */ /*>  : HashMap[Int, Set[I…  */ /*>  : HashMap[Int, Set[I…  */

rules.foreach { rule =>
  val after = rule.last.toInt
  val before = rule.head.toInt
  rulesMap.addBinding(after, before)
}

def func(next: List[Int], acc: List[Boolean]): List[Boolean] = {
  if (next.isEmpty) {
    acc
  } else {
    val head = next.head
    val tail = next.tail
    val notAllowed = rulesMap.get(head).getOrElse(Set.empty[Int])
    val res = !tail.exists(notAllowed.contains(_))
    func(tail, acc :+ res)
  }
}

// PART 1
produce
  .foldLeft(List.empty[List[Int]]) { case (acc, p) =>
    if (func(p, List.empty).forall(x => x)) acc :+ p else acc
  } /*>  : List[List[Int]] = List(List(83, 87, 21, 23, 12, 85, 27, 89, 63, 28, 97, 13, 81), List(55, 82, 85, 27, 89, 63, 28, 9…  */
  .map(i => i(i.size / 2))
  .sum /*>  : Int = 7198  */

// PART 2
produce
  .foldLeft(List.empty[List[Int]]) { case (acc, p) =>
    if (func(p, List.empty).forall(x => x)) acc else acc :+ p
  } /*>  : List[List[Int]] = List(List(83, 87, 21, 23, 12, 85, 27, 89, 63, 28, 97, 13, 81), List(55, 82, 85, 27, 89, 63, 28, 9…  */
  .map(i => i(i.size / 2))
  .sum /*>  : Int = 7198  */
