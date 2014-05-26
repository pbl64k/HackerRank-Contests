import scala.util.parsing.combinator._

object Solution {
    sealed abstract class Expr
    case class Var(val n: String) extends Expr
    case class App(val e1: Expr, val e2: Expr) extends Expr
    case class Lam(val n: String, val e: Expr) extends Expr

    // too slow.
    object LambdaParser extends RegexParsers {
        class ParseFailureException extends Exception

        def varn: Parser[Expr] = """\w+""".r ^^ (n => Var(n))

        def app: Parser[Expr] = ("(" ~> expr) ~! (expr <~ ")") ^^ {
            case (e1 ~ e2) => App(e1, e2)
        }

        def lam: Parser[Expr] = ("(\\" ~> (varn ~! varn.*) <~ ".") ~! (expr <~ ")") ^^ {
            case ((v ~ vars) ~ e) => {
                def f(v: Expr, e: Expr) = v match {
                    case Var(n) => Lam(n, e)
                    case _ => e
                }
                ((v :: vars) foldRight e)(f)
            }
            case _ => throw new ParseFailureException()
        }

        def expr: Parser[Expr] = varn | app | lam

        def apply(s: String) = parseAll(expr, s) match {
            case Success(r, _) => r
        }
    }

    def parse(x: String): Expr = parseExpr(x.toList)._1

    def parseExpr(cs: List[Char]): (Expr, List[Char]) = cs match {
        case '(' :: '\\' :: rest => parseLam(rest)
        case '(' :: rest => parseApp(rest)
        case _ => parseVar(cs)
    }

    def parseVar(cs: List[Char]): (Expr, List[Char]) = {
        @annotation.tailrec
        def aux(cs: List[Char], acc: List[Char]): (List[Char], List[Char]) = cs match {
            case Nil => (acc, cs)
            case c :: rest =>
                if ((c >= 'a' && c <= 'z') ||
                        (c >= 'A' && c <= 'Z') ||
                        (c >= '0' && c <= '9') ||
                        c == '_')
                    aux(rest, c :: acc)
                else
                    (acc, cs)
        }
        val (n, rest) = aux(cs, Nil)
        (Var(n.reverse mkString ""), rest)
    }

    def parseApp(cs: List[Char]): (Expr, List[Char]) = {
        val (e1, rest1) = parseExpr(cs)
        val (e2, rest2) = parseExpr(rest1.tail)
        (App(e1, e2), rest2.tail)
    }

    def parseLam(cs: List[Char]): (Expr, List[Char]) = {
        val (Var(n), rest1) = parseVar(cs)
        if (rest1.head == '.') {
            val (e, rest2) = parseExpr(rest1.tail.tail)
            (Lam(n, e), rest2.tail)
        }
        else {
            val (e, rest2) = parseLam(rest1.tail)
            (Lam(n, e), rest2)
        }
    }

    def neat(x: Expr): String = x match {
        case Var(n) => n
        case App(e1, e2 @ App(_, _)) => neat(e1) + "(" + neat(e2) + ")"
        case App(e1, e2) => neat(e1) + neat(e2)
        case Lam(n, e) => "\\" + n + ". (" + neat(e) + ")"
    }

    def free(x: String, expr: Expr): Boolean = expr match {
        case Var(z) => x == z
        case App(e1, e2) => free(x, e1) || free(x, e2)
        case Lam(n, e) => x != n && free(x, e)
    }

    def t(x: Expr): Expr = x match {
        case z @ Var(_) => z
        case App(e1, e2) => App(t(e1), t(e2))
        case Lam(n1, e @ Var(n2)) =>
                if (n1 == n2)
                    Var("I")
                else
                    App(Var("K"), e)
        case Lam(n1, e1 @ Lam(_, _)) =>
                if (free(n1, e1))
                    t(Lam(n1, t(e1)))
                else
                    App(Var("K"), t(e1))
        case Lam(n, e @ App(e1, e2)) => {
            def tt() = (free(n, e1), free(n, e2)) match {
                case (true, true) => App(App(Var("S"), t(Lam(n, e1))), t(Lam(n, e2)))
                case (true, false) => App(App(Var("C"), t(Lam(n, e1))), t(e2))
                case (false, true) => App(App(Var("B"), t(e1)), t(Lam(n, e2)))
                case (false, false) => App(Var("K"), t(e))
            }
            e2 match {
                case Var(x) =>
                        if (n == x && !free(n, e1))
                            t(e1)
                        else
                            tt()
                case _ => tt()
            }
        }
    }

    def main(args: Array[String]) = {
        val tn = readLine.toInt
        for (ix <- 1 to tn) {
            //val expr = LambdaParser(readLine)
            val expr = parse(readLine)
            println(neat(t(expr)))
        }
    }
}

