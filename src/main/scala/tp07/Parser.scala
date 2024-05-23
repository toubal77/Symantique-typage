package tp07

import com.sun.org.apache.xpath.internal.operations.Bool

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * prog --> '\z'
 *   |  cmd ";;"
 * cmd --> t
 *   |  val v '=' t
 * t --> open
 *   |  atom+ t?
 * open --> lambda x':' typ'.' t
 *   |  if t then t else t
 *   |  succ t
 *   |  pred t
 *   |  iszero t
 * atom --> x
 *   |  '('t')'
 *   |  true
 *   |  false
 *   |  0
 * typ --> atomTyp ("->" atomTyp)*
 * atomTyp --> '('typ')'
 *   | Bool
 *   | Nat
 * Indication : le non-terminal "typ" engendre un atomTyp suivi d'une séquence
 *  éventuellement vide de ('->' atomTyp). On gère cette situation de manière
 *  similaire à la séquence d'applications traitée la séance précédente. Cette
 *  fois, on fera appel à la méthode "Util.buildFctType" pour construire le
 *  type fonctionnel en cascade à partir d'une liste de types.
 */
class Parser extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|#.*)+""".r
  override val ident = """[a-zA-Z][a-zA-Z0-9]*""".r
  def keywords = Set("val")

  def prog : Parser[Term] = eof | cmd<~";;"
  def cmd : Parser[Term] = term | ("val"~>variable)~("="~>term) ^^ { case x~t => Val(x, t) }
  def eof = """\z""".r ^^ { _ => EOF }
  def term : Parser[Term] = open | atom ~ rep(atom) ~ opt(term) ^^ { case a ~ as ~ t => if (as.isEmpty) a else App(a :: as, t) }
  def open : Parser[Term] = lambda | cond | succ | pred | isZero
  def lambda : Parser[Term] = "lambda" ~> ident ~ (":" ~> typ) ~ ("." ~> term) ^^ { case x ~ ty ~ body => Lambda(x, ty, body) }
  def cond : Parser[Term] = "if" ~> term ~ ("then" ~> term) ~ ("else" ~> term) ^^ { case cond ~ t1 ~ t2 => Cond(cond, t1, t2) }
  def succ : Parser[Term] = "succ" ~> term ^^ { Succ(_) }
  def pred : Parser[Term] = "pred" ~> term ^^ { Pred(_) }
  def isZero : Parser[Term] = "iszero" ~> term ^^ { IsZero(_) }
  def atom : Parser[Term] = variable | "(" ~> term <~ ")" | tTrue | tFalse | zero
  def variable : Parser[Var] = ident ^^ { Var(_) }
  def tFalse : Parser[False.type] = "false" ^^ { _ => False }
  def tTrue : Parser[True.type] = "true" ^^ { _ => True }
  def zero : Parser[Zero.type] = "0" ^^ { _ => Zero }
  def typ : Parser[Typ] = atomTyp ~ rep("->" ~> atomTyp) ^^ { case ty ~ tys => Util.buildFctType(ty :: tys) }
  def atomTyp : Parser[Typ] = "(" ~> typ <~ ")" | bool | nat
  def bool : Parser[Bool] = "Bool" ^^ { _ => Bool }
  def nat : Parser[Nat] = "Nat" ^^ { _ => Nat }
}

