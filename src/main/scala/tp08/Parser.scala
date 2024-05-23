package tp08

import com.sun.org.apache.xpath.internal.operations.Bool
import tp08.{False, Nat, Term, True, Typ, U, Var, Zero}

import scala.util.parsing.combinator.JavaTokenParsers
import scala.collection.immutable.ListMap

/**
 * prog --> '\z'
 *   |  cmd ";;"
 * cmd --> seq
 *   |  val v '=' seq
 *   |  letrec f':' typ '=' t
 * seq --> t (';' t)*
 * t --> open
 *   |  atom+ t?
 * open --> fct
 *   |  if seq then seq else t
 *   |  succ t
 *   |  pred t
 *   |  iszero t
 *   |  let x = seq in t
 * fct --> lambda x':' typ'.' seq
 * atom --> x
 *   |  '('seq')'
 *   |  true
 *   |  false
 *   |  0
 *   |  unit
 * typ --> atomTyp ("->" atomTyp)*
 * atomTyp --> '('typ')'
 *   |  Bool
 *   |  Nat
 *   |  Unit
 */
class Parser extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|#.*)+""".r
  override val ident = """[a-zA-Z][a-zA-Z0-9]*""".r
  def keywords = Set("val")
  
  def prog : Parser[Term] = eof | cmd<~";;"
  def eof = """\z""".r ^^ { _ => EOF}
  def cmd : Parser[Term] = term | ("val"~>variable)~("="~>term) ^^ { case x~t => Val(x, t) }

  def seq: Parser[Term] = term ~ rep(";" ~> term) ^^ {
    case t ~ ts => ts.foldLeft(t)((acc, t) => App(acc, t))
  }
  def term : Parser[Term] = open | atom ~ rep(atom) ~ opt(term) ^^ { case a ~ as ~ t => if (as.isEmpty) a else App(a :: as, t) }

  def open: Parser[Term] = lambda | cond | succ | pred | isZero

  def lambda: Parser[Term] = "lambda" ~> ident ~ (":" ~> typ) ~ ("." ~> term) ^^ { case x ~ ty ~ body => Lambda(x, ty, body) }

  def cond: Parser[Term] = "if" ~> term ~ ("then" ~> term) ~ ("else" ~> term) ^^ { case cond ~ t1 ~ t2 => Cond(cond, t1, t2) }

  def succ: Parser[Term] = "succ" ~> term ^^ {
    Succ(_)
  }

  def pred: Parser[Term] = "pred" ~> term ^^ {
    Pred(_)
  }

  def isZero: Parser[Term] = "iszero" ~> term ^^ {
    IsZero(_)
  }

  def letIn: Parser[Term] = "let" ~> variable ~ ("=" ~> term) ~ ("in" ~> term) ^^ {
    case x ~ t1 ~ t2 => LetIn(x, t1, t2)
  }

  def atom: Parser[Term] = variable | "(" ~> term <~ ")" | tTrue | tFalse | zero

  def variable: Parser[Var] = ident ^^ {
    Var(_)
  }

  def tFalse: Parser[False.type] = "false" ^^ { _ => False }

  def tTrue: Parser[True.type] = "true" ^^ { _ => True }

  def zero: Parser[Zero.type] = "0" ^^ { _ => Zero }
  def u: Parser[Term] = "unit" ^^ { _ => U }

  def typ: Parser[Typ] = atomTyp ~ rep("->" ~> atomTyp) ^^ { case ty ~ tys => Util.buildFctType(ty :: tys) }

  def atomTyp: Parser[Typ] = "(" ~> typ <~ ")" | bool | nat

  def bool: Parser[Bool] = "Bool" ^^ { _ => Bool }

  //def nat: Parser[Nat] = "Nat" ^^ { _ => Nat }
  def unit = ???
}