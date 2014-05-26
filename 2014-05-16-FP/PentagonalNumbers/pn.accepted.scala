object Solution {
    def pn(x: Long): Long = (3L * x * x - x) / 2L

    def main(args: Array[String]) = {
        val t = readLine.toLong
        for (ix <- 0L until t) {
            println(pn(readLine.toLong))
        }
    }
}

