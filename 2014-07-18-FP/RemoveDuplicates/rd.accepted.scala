object Solution {
    def nb(str: String): String = {
        def f(acc: (Set[Char], List[Char]), c: Char): (Set[Char], List[Char]) = {
            if (acc._1 contains c)
                acc
            else
                (acc._1 + c, c :: acc._2)
        }

        (str foldLeft ((Set[Char](), List[Char]())))(f)._2.reverse mkString ""
    }

    def main(args: Array[String]) = {
        val str = readLine
        println(nb(str))
    }
}
