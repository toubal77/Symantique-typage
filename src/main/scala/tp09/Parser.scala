package tp09

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
 *   |  atom'.'label
 * fct --> lambda x':' typ'.' seq
 * atom --> x
 *   |  '('seq')'
 *   |  true
 *   |  false
 *   |  0
 *   |  unit
 *   |  '{' fields? '}'
 * fields --> label '=' seq (',' fields)?
 * typ --> atomTyp ("->" atomTyp)*
 * atomTyp --> '('typ')'
 *   |  Bool
 *   |  Nat
 *   |  Unit
 *   |  '{' fieldsTyp? '}'
 * fieldsTyp --> label ':' typ (',' fieldsTyp)?
 */
class Parser extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|#.*)+""".r
  override val ident = """[a-zA-Z][a-zA-Z0-9]*""".r
  def keywords = Set("val")

  def prog: Parser[Term] = eof | cmd <~ ";;"
  def eof: Parser[Term] = """\z""".r ^^ { _ => EOF }

  def cmd: Parser[Term] = seq | valCmd | letRec

  def seq: Parser[Term] = term ~ rep(";" ~> term) ^^ {
    case t ~ ts => ts.foldLeft(t)((acc, t) => App(acc, t))
  }

  def valCmd: Parser[Term] = "val" ~> ident ~ ("=" ~> seq) ^^ {
    case x ~ t => Val(Var(x), t)
  }

  def letRec: Parser[Term] = ???

  def term: Parser[Term] = open | atom

  def open: Parser[Term] = lambda | cond | succ | pred | isZero | letIn | proj

  def lambda: Parser[Term] = "lambda" ~> ident ~ (":" ~> typ) ~ ("." ~> seq) ^^ {
    case x ~ typ ~ body => Abs(Var(x), typ, body)
  }

  def cond: Parser[Term] = "if" ~> seq ~ ("then" ~> seq) ~ ("else" ~> term) ^^ {
    case cond ~ thenBranch ~ elseBranch => Cond(cond, thenBranch, elseBranch)
  }

  def succ: Parser[Term] = "succ" ~> term ^^ { t => Succ(t) }

  def pred: Parser[Term] = "pred" ~> term ^^ { t => Pred(t) }

  def isZero: Parser[Term] = "iszero" ~> term ^^ { t => IsZero(t) }

  def letIn: Parser[Term] = "let" ~> ident ~ ("=" ~> seq) ~ ("in" ~> seq) ^^ {
    case x ~ t1 ~ t2 => LetIn(Var(x), t1, t2)
  }

  def proj: Parser[Term] = atom ~ ("." ~> ident) ^^ {
    case t ~ label => Proj(t, label)
  }

  def atom: Parser[Term] = variable | tTrue | tFalse | zero | u | record

  def variable: Parser[Term] = ident ^^ { x => Var(x) }

  def tTrue: Parser[Term] = "true" ^^ { _ => True }

  def tFalse: Parser[Term] = "false" ^^ { _ => False }

  def zero: Parser[Term] = "0" ^^ { _ => Zero }

  def u: Parser[Term] = "unit" ^^ { _ => U }

  def record: Parser[Term] = "{" ~> fields <~ "}" ^^ { fields => Rec(fields.toList) }

  def fields: Parser[List[(String, Term)]] = repsep(ident ~ ("=" ~> seq), ",") ^^ {
    case pairs => pairs.map { case label ~ term => (label, term) }
  }

  def typ: Parser[Typ] = atomTyp ~ rep("->" ~> atomTyp) ^^ {
    case typ ~ Nil => typ
    case typ ~ typs => typs.foldLeft(typ)((acc, t) => Arrow(acc, t))
  }

  def atomTyp: Parser[Typ] = bool | nat | unit | recordTyp | "(" ~> typ <~ ")"

  def bool: Parser[Typ] = "Bool" ^^ { _ => BoolType }

  def nat: Parser[Typ] = "Nat" ^^ { _ => Nat }

  def unit: Parser[Typ] = "Unit" ^^ { _ => UnitType }

  def recordTyp: Parser[Typ] = "{" ~> fieldsTyp <~ "}" ^^ { fields => RecordType(fields) }

  def fieldsTyp: Parser[ListMap[String, Typ]] = repsep(ident ~ (":" ~> typ), ",") ^^ {
    case pairs => ListMap(pairs.map { case label ~ typ => (label, typ) }: _*)
  }
}
