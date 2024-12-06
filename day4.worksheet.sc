import scala.collection.mutable.ArraySeq
val day4input = os.read
  .lines(os.pwd / "inputs" / "day4.txt")
  .map(
    _.to(IndexedSeq)
  ) /*>  : IndexedSeq[IndexedSeq[Char]] = ArraySeq(MMSMXMSASXMXAXMAXSASXXXMASAMXSMMMXMMXMXMASMXMASMSMXSSXMASXSMMSASMSAXAMXMAMM…  */

def clockworkSearch(
    block: IndexedSeq[IndexedSeq[Char]],
    x: Int,
    y: Int,
    xLimit: Int,
    yLimit: Int,
    search: String
) = {
  val directions = List(
    (0, 1),
    (1, 0),
    (1, 1),
    (1, -1),
    (-1, 1),
    (-1, -1),
    (0, -1),
    (-1, 0)
  )
  if (block(y)(x) == search.charAt(0)) {
    directions.foldLeft(List.empty[String]) { case (acc, (dx, dy)) =>
      val found = Range(0, search.size).foldLeft("") { case (acc, i) =>
        val nx = x + dx * i
        val ny = y + dy * i
        if (nx >= 0 && nx < xLimit && ny >= 0 && ny < yLimit) {
          acc + block(ny)(nx)
        } else {
          acc
        }
      }
      if (found == search) {
        acc :+ search
      } else {
        acc
      }
    }
  } else {
    List.empty[String]
  }
}

// PART 1
val xLimit = day4input(0).size /*>  : Int = 140  */
val yLimit = day4input.size /*>  : Int = 140  */

Range(0, yLimit).flatMap { y =>
  Range(0, xLimit).flatMap { x =>
    clockworkSearch(day4input, x, y, xLimit, yLimit, "XMAS")
  }
}.size /*>  : Int = 2603  */

// PART 2
def crossSearch(
    block: IndexedSeq[IndexedSeq[Char]],
    x: Int,
    y: Int,
    xLimit: Int,
    yLimit: Int,
    search: String
) = {
  val relCords = List(
    List((-1, -1), (0, 0), (1, 1)),
    List((-1, 1), (0, 0), (1, -1))
  )
  if (block(y)(x) == search.charAt(1)) {
    relCords
      .map { cords =>
        cords.foldLeft("") { case (acc, (dx, dy)) =>
          val nx = x + dx
          val ny = y + dy
          if (nx >= 0 && nx < xLimit && ny >= 0 && ny < yLimit) {
            acc + block(ny)(nx)
          } else {
            acc
          }
        }
      }
      .forall(str => str == search || str == search.reverse)
  } else {
    false
  }
}

Range(0, yLimit)
  .flatMap { y =>
    Range(0, xLimit).map { x =>
      crossSearch(day4input, x, y, xLimit, yLimit, "MAS")
    }
  }
  .filter(x => x)
  .size /*>  : IndexedSeq[Boolean] = Vector(false, false, false, false, false, false, false, false, false, false, false, false, fals…  */
