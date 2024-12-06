val day2Input = os.read
  .lines(os.pwd / "inputs" / "day2.txt")
  .map(
    _.split(" ").map(_.toInt).toList
  )
  .toList /*>  : List[List[Int]] = List(List(6, 8, 9, 10, 12, 13, 12), List(76, 77, 80, 82, 84, 86, 88, 88), List(84, 86, 88, …  */

def isSafe(row: List[Int]): Boolean = {
  if (row == row.sorted || row == row.sorted.reverse) {
    row.sliding(2).forall { case List(a, b) =>
      math.abs(a - b) >= 1 && math.abs(a - b) <= 3
    }
  } else {
    false
  }
}

// PART 1
val d1p1Ans = day2Input
  .map { row =>
    isSafe(row)
  }
  .filter(x => x)
  .size /*>  : Int = 502  */

// PART 2
day2Input
  .map { row =>
    isSafe(row) || {
      Range(0, row.size)
        .map { i =>
          val newRow = row.toBuffer
          newRow.remove(i)
          isSafe(newRow.toList)
        }
        .exists(x => x)
    }
  }
  .filter(x => x)
  .size /*>  : Int = 544  */

// printAns(Some(d1p1Ans), Some(d1p2Ans))
