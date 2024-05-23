package tp06

import java.io.FileReader
import java.io.PrintStream
import java.io.File
import scala.util.parsing.input.StreamReader
import scala.util.parsing.input.Reader
import scala.Console._
import Util._

class Interpretor {
  // notre perser
  val parser = new Parser
  // notre evaluateur
  val evaluator = new Evaluator
  // pour le formatage
  val formatter = new Formatter
  
  def interpret(reader : Reader[Char]) : Unit = {
    // le fichier du résultat
    System.setOut(new PrintStream(new File("src/tp06/results.txt")))
    // intreprete le contenu du fichier exemples.txt
    interpret(reader, List())
  }

  /**
   * Interprète le flux fourni par reader dans le contexte ctx.
   * Une analyse syntaxique du flux est réalisée par le parser pour construire
   *  les termes associés aux commandes du flux.
   * Au fur et à mesure, le contexte (une liste de variables globales et leur
   *  valeur) est injecté dans les termes et, pour chaque terme clos obtenu
   *  après injection, les opérations suivantes sont réalisées :
   * - évaluation du terme
   * - formatage du terme obtenu par l'évaluation
   * - affichage du terme formaté
   * Le contexte est enrichi au fil de l'interprétation, chaque fois qu'un
   *  terme de la forme "val x = t" est rencontré.
   * Un message signale l'apparition d'un terme non-clos dans le flux. Un tel
   *  terme n'est pas évalué.
   * Un message d'erreur est produit en cas d'erreur syntaxique repérée par le
   *  parser.
   * Indication : pour alléger cette méthode, on fera appel à la méthode
   *  "interpret(t: Term)".
   */
  private def interpret(reader : Reader[Char], ctx : List[Alias]) : Unit = {
   // match le resultat de perse avec perser.prog
    parser.parse(parser.prog, reader) match {
      // fin de fichier, on sort
      case parser.Success(EOF, _) => 
      // si success 
      case parser.Success(matched, t) => {
        // appel récursive à la fonction interpret avec nouvel contx
        // avec l'ajouter de l'alias en tête du contexte ctx
        interpret(t, buildNewCtx(ctx, interpret(inject(ctx, matched.asInstanceOf[Term]))))
      }
      // si failure, on affiche le message avec le status FAILURE
      case parser.Failure(msg, _) =>{
        println("FAILURE:" + msg)
      }
      // si ERROR, on affiche le message avec le status ERROR
      case parser.Error(msg, _) => println("ERROR:" + msg)
    }
  }
    
  /**
   * Teste si t est un terme clos.
   * Si la réponse est négative, on formate le terme que l'on affiche sur la
   *  sortie standard et on signale qu'il est non clos.
   * Sinon, on évalue t, on affiche le terme bloqué obtenu et on le retourne.
   */
  private def interpret(t: Term) : Option[Term] = 
    // si t n'est pas un terme clos.
    if (!isClosed(t)) 
    {
      // on formate le terme que l'on affiche sur la sortie standard
      println(formatter.format(t) + " : Terme non clos !\r\n")
      // on signale qu'il est non clos avec le none de type option
      None
    } 
    // si t est un terme clos.
    else 
    {
      // on évalue t
      val value = evaluator.evaluate(t)
      // on affiche le terme bloqué obtenu
      println(formatter.format(value) + "\r\n")
      // on le signale avec la méthode some
      Some(value)
    }
}

object Main {
  def main(args : Array[String]) {
    // stocker le contenu du fichier exemples.txt
    // dans la valeur reader
    val reader = StreamReader(new FileReader("src/tp06/examples.txt"))
    
    // appel à la fonction interpte avec le contenu 
    // du fichier exemple.txt
    new Interpretor().interpret(reader)
  }
}