
object RomanArabicConverter extends Application {

    val conversion = List(("M",  1000),
                          ("CM", 900),
                          ("D",  500),
                          ("CD", 400),
                          ("C",  100),
                          ("XC", 90),
                          ("L",  50),
                          ("XL", 40),
                          ("X",  10),
                          ("IX", 9),
                          ("V",  5),
                          ("IV", 4),
                          ("I",  1))

    def roman_to_arabic(roman: String): Int = {
        //TODO Input validation (valid roman number < 4000)

        var last = 0
        def handleNext(c: Char, acc: Int): Int = {
            val n = conversion.find(_._1 == c.toString()).get._2
            val newAcc = if (n >= last) acc + n else acc - n
            last = n
            return newAcc
        }

        roman.foldRight[Int] (0)(handleNext)
    }

    def arabic_to_roman(arabic: Int): String = {
        //TODO Input validation (0 < _ < 4000)

        def compute(acc: String, remainder: Int, tuples: List[Tuple2[String, Int]]): String = tuples match {
            case List() => acc
            case (r, a) :: tail => if (remainder >= a) compute(acc + r, remainder - a, tuples)
                                   else compute(acc, remainder, tail)
        }

        compute("", arabic, conversion)
    }

}