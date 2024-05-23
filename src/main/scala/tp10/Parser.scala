package tp10

import scala.util.parsing.combinator.JavaTokenParsers
import Util._
import scala.collection.immutable.ListMap

/**
 * prog --> '\z'
 *   |  cmd ";;"
 * cmd --> seq
 *   |  val v '=' seq
 *   |  letrec f':' typ '=' fct 
 * seq --> t (';' seq)?
 * t --> open
 *   |  atom t?
 * open --> fct
 *   |  if seq then seq else t
 *   |  succ t
 *   |  pred t
 *   |  iszero t
 *   |  let x = seq in t
 *   |  atom'.'label
 *   |  ref t
 *   |  '!'t
 *   |  atom := t
 * fct --> lambda x':' typ'.' seq
 * atom --> x
 *   |  '('seq')'
 *   |  true
 *   |  false
 *   |  0
 *   |  unit
 *   |  '{' fields? '}'
 * fields --> label '=' seq (',' fields)?
 * typ --> atomTyp ("->" typ)?
 * atomTyp --> '('typ')'
 *   |  Bool
 *   |  Nat
 *   |  Unit
 *   |  '{' fieldsTyp? '}'
 *   |  Ref typ
 * fieldsTyp --> label ':' typ (',' fieldsTyp)?
 */
class Parser extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|#.*)+""".r
  override val ident = """[a-zA-Z][a-zA-Z0-9]*""".r
  def keywords = "lambda".r | "true".r | "false".r | "if".r | "then".r |
    "else".r | "val".r | "succ".r | "pred".r | "iszero".r | "unit".r |
    "Unit".r | "let".r |"in".r | "Bool".r | "Nat".r | "letrec".r | "ref".r |
    "Ref".r
  
  def prog : Parser[Term] = ( eof | cmd<~";;" )
  def eof = """\z""".r ^^ { _ => EOF}
  def cmd : Parser[Term] = ???
}