package tp05

import scala.util.parsing.combinator.RegexParsers

/**
 * t --> '('t')'
 *    |  '\z'
 *    |  false
 *    |  true
 *    |  if t then t else t
 *    |  0
 *    |  succ t
 *    |  pred t
 *    |  iszero t
 */
class Parser extends RegexParsers {
  def term : Parser[Term] = parenTerm | eof
  def parenTerm = ("("~>term<~")")
  def eof = """\z""".r ^^ { _ => EOF }
  def _true = "true".r ^^ { _ => TRUE }
  def _false = "false".r ^^ { _ => FALSE }
  def _zero  = """(0*)""".r ^^ { _ => ZERO }

  def ifThenElseTerm: Parser[Term] = "if" ~> term ~ "then" ~ term ~ "else" ~ term ^^ {
    case condition ~ _ ~ thenBranch ~ _ ~ elseBranch => IfThenElse(condition, thenBranch, elseBranch)
  }

  def succTerm: Parser[Term] = "succ" ~> term ^^ { t => Succ(t) }

  def predTerm: Parser[Term] = "pred" ~> term ^^ { t => Pred(t) }

  def iszeroTerm: Parser[Term] = "iszero" ~> term ^^ { t => IsZero(t) }
}
