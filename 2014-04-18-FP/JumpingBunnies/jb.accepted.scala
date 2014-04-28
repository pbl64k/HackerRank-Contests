object Solution {
    @annotation.tailrec
    def gcd(a: BigInt, b: BigInt): BigInt =
        if (b == 0)
            a
        else if (a < b)
            gcd(b, a)
        else
            gcd(b, a % b)

    def lcm(a: BigInt, b: BigInt): BigInt = (a / gcd(a, b)) * b

    def main(args: Array[String]) = {
        readLine
        val xs = readLine.split(" ").map((x: String) => BigInt(x.toInt))
        println((xs foldLeft BigInt(1))(lcm))
    }
}
