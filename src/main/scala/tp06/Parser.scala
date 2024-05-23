package tp06

import scala.util.parsing.combinator.JavaTokenParsers
import tp06.Util._

/**
 * prog --> '\z'
 *   | cmd ";;"
 * cmd --> term
 *   |  val v '=' term
 * term --> lambda x'.' term
 *   |  atom term?
 * atom --> x
 *   | '('term')'
 *   
 * Indication : le non-terminal "term" produit soit une lambda-abstraction que
 *  l'on traite normalement, soit une séquence d'"atom" terminée par une
 *  lambda-abstraction optionnelle.
 *  Une solution pour gérer ce cas est d'introduire un "Parser[[List[Term]]"
 *  qui produira une liste de termes que l'on transformera en une séquence
 *  d'applications grâce à la méthode "Util.buildApp".
 *  
 * Indication : les variables respectent le motif [a-zA-Z][a-zA-Z0-9]* et ne
 *  doivent pas être des mots clés du langage.
 */
class Parser extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|#.*)+""".r
  override val ident = """[a-zA-Z][a-zA-Z0-9]*""".r
  // keywords
  def keywords = "lambda|val".r
  
  // prog
  // prog --> '\z'| cmd ";;"
  def prog : Parser[Term] = ( eof | cmd<~";;" )
  
  // cmd
  // cmd --> term |  val v '=' term
  def cmd : Parser[Term] = ( term
      | ("val"~>variable)~("="~>term) ^^ { case Var(x)~t => Val(Var(x), t) }
  )
  // eof
  def eof = """\z""".r ^^ { _ => EOF}
  
  // term
  // term --> lambda x'.' term |  atom term?
  def term : Parser[Term] =
      (("lambda"~>variable<~".") ~ term ^^ { case Var(x) ~ t => Abs(Var(x), t) }).|(
          (atom.+ ~ term.?) ^^ { case t1 ~ Some(t2) => App(buildApp(t1), t2)
                                 case t1 ~ None => buildApp(t1)})
  // atom
  // atom --> x | '('term')'
  def atom : Parser[Term] = (variable
      | "("~>term<~")" ^^ { case t => t })
  
  // variable
  // le vaiable ne commence pas par lambda | val
  def variable : Parser[Var] = (not(keywords)~>ident) ^^ { s => Var(s)}
}
