package scales
import util.parsing.combinator._
import exprs._

class Comp extends RegexParsers with PackratParsers {

  //Constants

  lazy val integer:PackratParser[Expr] = """-?\d+""".r ^^ {
    s => Constant("Int", s)
  }

  def bool:PackratParser[Expr] = "(true)|(false)".r  ^^ {
    case "true" => Constant("Bool", "1")
    case "false" => Constant("Bool", "0")
  }

  def null_const:PackratParser[Expr] = "null".r  ^^ {
    a => Constant("Object", "null")
  }

  def str_const:PackratParser[Expr] = "\"[^\"]*\"".r  ^^ {
    s => Constant("String", s)
  }

  //Var stuff
  def varname:PackratParser[String] = "[A-Za-z][A-Za-z0-9_]*".r  ^^ {
    s => s
  }

  def ident:PackratParser[Expr] = varname  ^^ {
    s => Var(s)
  }

  def asgn:PackratParser[Expr] = varname ~ ("<-" ~> expr)  ^^ {
    case a ~ b => Asgn(a, b)
  }

  def arrget:PackratParser[Expr] = ident ~ ("[" ~> expr <~ "]") ^^ {
    case a ~ b => ArrGet(a, b)
  }

  def arrdec:PackratParser[Expr] = "new" ~> "Int" ~> "[" ~> expr <~ "]" ^^ {
    a => ArrDec(a)
  }

  def classdec:PackratParser[Expr] = "new" ~> varname ~ ( "(" ~> exprlist <~ ")" ) ^^ {
    case a ~ b => ClassDec(a, b)
  }

  def arrasgn:PackratParser[Expr] = varname ~ ("[" ~> expr <~ "]" <~ "<-") ~ expr  ^^ {
    case a ~ b ~ c => ArrAsgn(a, b, c)
  }

  def parens:PackratParser[Expr] = "(" ~> expr <~ ")" 

  //Methods and objects

  def call:PackratParser[Expr] = varname ~ ("(" ~> exprlist <~ ")") ^^ {
    case (a ~ b) => MethodCall(a, b)
  }

  def classcall:PackratParser[Expr] = (expr <~ ".") ~ varname ~ ("(" ~> exprlist <~ ")") ^^ {
    case a ~ b ~ c => ClassCall(a, b, c)
  }

  // Arithmetic

  lazy val add: PackratParser[Expr] = expr ~ "+" ~ expr ^^ {
    case(a ~ "+" ~ b) => OpExpr(OP.PLUS, a, b)
  }

  lazy val div: PackratParser[Expr] = expr ~ "/" ~ expr ^^ {
    case(a ~ "/" ~ b) => OpExpr(OP.DIV, a, b)
  }

  lazy val mult: PackratParser[Expr] = expr ~ "*" ~ expr ^^ {
    case(a ~ "*" ~ b) => OpExpr(OP.MULT, a, b)
  }

  lazy val sub: PackratParser[Expr] = expr ~ "-" ~ expr ^^ {
    case(a ~ "-" ~ b) => OpExpr(OP.MINUS, a, b)
  }

  //Booleans

  lazy val cmp: PackratParser[Expr] = "~" ~> expr ^^ {
    a => UnaOp(OP.CMP, a)
  }

  lazy val not: PackratParser[Expr] = "not" ~> expr ^^ {
    a => UnaOp(OP.NOT, a)
  }

  lazy val isvoid: PackratParser[Expr] = "isvoid" ~> expr ^^ {
    a => UnaOp(OP.VOID, a)
  }

  lazy val les: PackratParser[Expr] = (expr <~ "<") ~ expr ^^ {
    case(a ~ b) => OpExpr(OP.LT, a, b)
  }

  lazy val leq: PackratParser[Expr] = (expr <~ "<=") ~ expr ^^ {
    case(a ~ b) => OpExpr(OP.LE, a, b)
  }

  lazy val neq: PackratParser[Expr] = (expr <~ "<>") ~ expr ^^ {
    case(a ~ b) => OpExpr(OP.NE, a, b)
  }

  lazy val gre: PackratParser[Expr] = (expr <~ ">") ~ expr ^^ {
    case(a ~ b) => OpExpr(OP.GT, a, b)
  }

  lazy val geq: PackratParser[Expr] = (expr <~ ">=") ~ expr ^^ {
    case(a ~ b) => OpExpr(OP.GE, a, b)
  }

  lazy val eql: PackratParser[Expr] = (expr <~ "=") ~ expr ^^ {
    case(a ~ b) => OpExpr(OP.EQ, a, b)
  }

  //Control structures
  lazy val ifelse: PackratParser[Expr] = (
    "if" ~> expr ~ ("then" ~> expr) ~ ("else" ~> expr <~ "fi")) ^^ {
    case a ~ b ~ c => If(a, b, c) 
  }

  lazy val letasgn: PackratParser[Let] = (
    varname ~ (":" ~> typename <~ "<-") ~ expr) ^^ {
    case i ~ t ~ e => Let(i, t, Some(e))
  }

  lazy val letpln: PackratParser[Let] = (
    varname ~ (":" ~> typename)) ^^ {
    case i ~ t => Let(i, t, None)
  }

  lazy val letlist: PackratParser[List[Let]] = repsep(letasgn | letpln, ",")

  lazy val lets: PackratParser[Expr] = (
    "let" ~> letlist ~ ("in" ~> expr <~ "tel")) ^^ {
    case ls ~ e => LetX(ls, e)
  }

  lazy val exprlist: PackratParser[List[Expr]] = repsep(expr, ",")

  lazy val seq: PackratParser[Expr] = "{" ~> repsep(expr, ";") <~ "}" ^^ {
    s => Seq(s)
  }

  lazy val loop: PackratParser[Expr] = (
    ("while" ~> expr <~ "loop") ~ expr <~ "pool") ^^ {
    case(g ~ b) => While(g, b)
  }

  lazy val expr: PackratParser[Expr] = (
    add | sub | mult | div | cmp |
    not | les | leq | neq | gre | geq |eql | isvoid |
    ifelse | call | classcall | classdec | 
    integer | bool | parens | str_const | null_const |
    lets | loop | seq | //ifelse | 
    asgn | arrasgn | arrdec | arrget | ident 
  )

  lazy val typename: PackratParser[String] = "([A-Z][A-Za-z0-9_]*)".r ^^ {
    s => s
  } 

  lazy val args: PackratParser[List[Typed]] = repsep(typedVar, ",")

  lazy val method: PackratParser[Method] = (
    varname ~ ("(" ~> args <~ ")" <~ ":") ~ typename ~ (expr)) ^^ {
    case s ~ f ~ t ~ e => Method(s, f, t, e)
  }
    
  lazy val typedVar: PackratParser[Typed] = (
    varname ~ (":" ~> "Int" ~> "[" ~> "]") |
    varname ~ (":" ~> typename)) ^^ {
    case s ~ "]" => Typed(s, "Int[]")
    case s ~ t => Typed(s, t)
  }

  lazy val attribute: PackratParser[Attribute] = (
    varname ~ (":" ~> "Int" ~> "[" ~> "]") |
    varname ~ (":" ~> typename)) ^^ {
    case s ~ "]" => Attribute(s, "Int[]", None)
    case s ~ t => Attribute(s, t, None)
  }

  lazy val definedAttribute: PackratParser[Attribute] = (
    varname ~ (":" ~> "Int" ~> "[" ~> "]") ~ ("<-" ~> expr) |
    varname ~ (":" ~> typename) ~ ("<-" ~> expr)) ^^ {
    case s ~ "]" ~ e => Attribute(s, "Int[]", Some(e))
    case s ~ t ~ e => Attribute(s, t, Some(e))
  }

  lazy val features: PackratParser[List[Feature]] = (
    rep((definedAttribute | attribute | method) <~ ";"))

  lazy val cls: PackratParser[Cls] = (
    "class" ~> typename ~ ("{" ~> features <~ "}"))  ^^{
    case a ~ b => new Cls(a, "UCObject", b)
  }

  lazy val subcls: PackratParser[Cls] = (
    "class" ~> typename ~ ("inherits" ~> typename <~ "{") ~ features <~ "}") ^^ {
    case a ~ b ~ c => new Cls(a, b, c)
  }

  lazy val prog: PackratParser[List[Cls]] = rep(cls | subcls)

}

object Uncool extends Comp {
  def make_ast(file: String) : List[Cls]= {
    val sauce = io.Source.fromFile(file).mkString
//    val sauce = io.Source.stdin.mkString
    parseAll(prog, sauce).get
  }
}
