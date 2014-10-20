import scala.annotation._

object Solution {
    @tailrec
    def slv(pfx: List[Char], a: List[Char], b: List[Char]): (List[Char], List[Char], List[Char]) = {
        if (!a.isEmpty && !b.isEmpty && a.head == b.head)
            slv(a.head :: pfx, a.tail, b.tail)
        else
            (pfx, a, b)
    }

    def main(args: Array[String]) = {
        val astr = readLine
        val bstr = readLine
        val (a, b, c) = slv(Nil, astr.toList, bstr.toList)
        println(a.length.toString + " " + (a.reverse mkString ""))
        println(b.length.toString + " " + (b mkString ""))
        println(c.length.toString + " " + (c mkString ""))
    }
}

