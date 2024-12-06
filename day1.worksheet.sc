val day1Input = os.read
  .lines(os.pwd / "inputs" / "day1.txt")
  .map(
    _.split("   ").map(_.toInt).toList
  )
  .toList /*>  : List[List[Int]] = List(List(98415, 86712), List(21839, 96206), List(14958, 40971), List(91380, 63825), List(4…  */

val left = day1Input.map(
  _.head
) /*>  : List[Int] = List(98415, 21839, 14958, 91380, 48482, 47618, 69295, 73790, 95008, 30041, 46680, 12873, 47722, 81092, 64…  */
val right = day1Input.map(
  _.last
) /*>  : List[Int] = List(86712, 96206, 40971, 63825, 40971, 98239, 72055, 20692, 91254, 73739, 55405, 14682, 74915, 16759, 31…  */

val ordered = left.sorted.zip(
  right.sorted
) /*>  : List[Tuple2[Int, Int]] = List((10029,10138), (10060,10152), (10113,10331), (10126,10409), (10147,10695), (10215,10730…  */
// PART 1
ordered.map { case (l, r) => math.abs(r - l) }.sum /*>  : Int = 2192892  */

// PART 2
val occurances = right
  .groupBy(x => x)
  .view
  .mapValues(x => x.length)
  .toMap /*>  : Map[Int, Int] = HashMap(40307 -> 1, 71049 -> 1, 50682 -> 13, 32501 -> 1, 96206 -> 1, 51077 -> 1, 56339 -> 1, 1…  */

left
  .map(x => x * occurances.getOrElse(x, 0))
  .sum /*>  : Int = 22962826  */
