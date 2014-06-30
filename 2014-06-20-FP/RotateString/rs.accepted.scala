object Solution {
    def f(x: List[Char]): Stream[String] = {
        def aux(b: List[Char], f: List[Char]): Stream[String] = {
            val s = (f ++ b.reverse) mkString ""
            if (f.isEmpty)
                Stream(s)
            else
                s #:: aux(f.head :: b, f.tail)
        }
        aux(List(x.head), x.tail)
    }

    def main(args: Array[String]) = {
        val t = readLine.toInt
        for (ix <- 1 to t) {
            val s = readLine.toList
            println(f(s) mkString " ")
        }
    }
}
