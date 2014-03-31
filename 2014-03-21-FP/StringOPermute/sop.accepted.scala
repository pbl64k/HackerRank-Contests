object Solution {
    @annotation.tailrec
    def f[T](s: List[T], acc: List[T]): List[T] = s match {
        case a :: (b :: ss) => f(ss, a :: (b :: acc))
        case a :: Nil => f(Nil, a :: acc)
        case Nil => acc.reverse
    }

    def main(args: Array[String]) {
        val t = readLine.toInt
        for (ix <- 0 until t) {
            val str = readLine.toList
            println(f(str, Nil) mkString "")
        }
    }
}
