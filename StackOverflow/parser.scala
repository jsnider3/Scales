import util.parsing.combinator._

abstract class Arith

case class Add(x: Arith, y: Arith) extends Arith
case class Sub(x: Arith, y: Arith) extends Arith
case class Mul(x: Arith, y: Arith) extends Arith
case class Div(x: Arith, y: Arith) extends Arith
case class Num(x: Int) extends Arith

class Comp extends RegexParsers with PackratParsers {

  //Constants

  lazy val integer:PackratParser[Num] = """-?\d+""".r ^^ {
    s => Num(s.toInt)
  }

  // Arithmetic

  lazy val add: PackratParser[Arith] = expr ~ "+" ~ expr ^^ {
    case(a ~ "+" ~ b) => Add(a, b)
  }

  lazy val div: PackratParser[Arith] = expr ~ "/" ~ expr ^^ {
    case(a ~ "/" ~ b) => Div(a, b)
  }

  lazy val mult: PackratParser[Arith] = expr ~ "*" ~ expr ^^ {
    case(a ~ "*" ~ b) => Mul(a, b)
  }

  lazy val sub: PackratParser[Arith] = expr ~ "-" ~ expr ^^ {
    case(a ~ "-" ~ b) => Sub(a, b)
  }

  lazy val expr: PackratParser[Arith] = (
    mult | div |add | sub |
    integer 
  )

}

object Uncool extends Comp {
  def main(args: Array[String]) = {
    println(parseAll(expr, "42*2 + 8 / 3 - 2"))
  }
}
