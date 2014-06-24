object Solution {
    def main(args: Array[String]) = {
        val n = readLine.toInt
        val xs = readLine.split(" ").map(_.toInt).toArray
        val q = readLine.toInt
        for (ix <- 1 to q) {
            val List(x, y) = readLine.split(" ").map(_.toInt).toList
            if (x > y || xs(x - 1) % 2 == 1 || (x < n && xs(x) == 0)) {
                println("Odd")
            }
            else {
                println("Even")
            }
        }
    }
}
