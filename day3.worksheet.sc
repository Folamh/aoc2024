val day3input =
  os.read(
    os.pwd / "inputs" / "day3.txt"
  ) /*>  : String =   */ /*>  : String = why()}''(!how()$~mul(420,484) ]}}mul(218,461),]…  */ /*>  : String = why()}''(!how()$…  */ /*>  : String = why()}''(…  */ /*>  : String = why()}''(…  */ /*>  : String = why()}''(…  */ /*>  : String = why()}''(…  */ /*>  : String = why()}''(…  */ /*>  : String = why()}''(…  */ /*>  : String = why()}''(…  */ /*>  : String = why()}''(…  */ /*>  : String = why()}''(…  */ /*>  : String = why()}''(…  */ /*>  : String = why()}''(…  */ /*>  : String = why()}''(…  */ /*>  : String = why()}''(…  */ /*>  : String = why()}''(…  */

val regex = """mul\((\d+),(\d+)\)""".r /*>  : Regex = mul\((\d+),(\d+)\)  */

// Part 1
regex
  .findAllMatchIn(day3input)
  .map { m =>
    m.group(1).toInt * m.group(2).toInt
  }
  .sum /*>  : Int = 174561379  */

// Part 2
val dontSplit =
  day3input
    .split("don't()")
    .toList /*>  : Array[String] = [Ljava.lang.String;@4122f499  */ /*>  : List[String] = List(why()}''(!how()$~mul(420,484) ]…  */ /*>  : List[String] = Lis…  */ /*>  : List[String] = Lis…  */ /*>  : List[String] = Lis…  */ /*>  : List[String] = Lis…  */ /*>  : List[String] = Lis…  */

val newInstructions = dontSplit.head + dontSplit.tail
  .flatMap(ds => ds.split("do()").toList.tail)
  .mkString /*>  : String = why()}''(!how()$~mul(420,484) ]}}mul(218,461),]/select()mul(93,56)';$-;*#$mul(162,415)mul(556,374)…  */

regex
  .findAllMatchIn(newInstructions)
  .map { m =>
    m.group(1).toInt * m.group(2).toInt
  }
  .sum
