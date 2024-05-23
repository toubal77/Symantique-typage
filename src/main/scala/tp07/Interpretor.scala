package tp07

import java.io.FileReader
import java.io.PrintStream
import java.io.File
import scala.util.parsing.input.StreamReader
import scala.util.parsing.input.Reader
import scala.Console._
import Util._
import TypeChecker._

import java.io.FileOutputStream
import scala.annotation.tailrec

class Interpretor {
  val parser = new Parser
  val evaluator = new Evaluator
  val typeChecker = new TypeChecker
  val formatter = new Formatter

  def interpret(reader: Reader[Char]): Unit = {
    Console.withOut(new PrintStream(new File("src/test/scala/tp07/results.txt"))) {
      interpret(reader, List())
    }
  }

  /**
   * Interprète le flux fourni par reader dans le contexte ctx.
   * Les termes produits au fur et à mesure de l'analyse syntaxique sont :
   *  - vérifiés clos ;
   *  - typés ;
   *  - évalués, le résultat étant produit sur la sortie standard.
   * Si erreur survient au cours d'une des trois premières étapes elle est
   *  signalée sur la sortie standard (rq : aucune erreur ne peut survenir
   *  à l'évaluation, puisque le langage a été montré sûr en cours).
   * Indication : pour alléger cette méthode, on fera appel à la méthode
   *  "interpret(t: Term)".
   */
  @tailrec
  private def interpret(reader: Reader[Char], ctx: Context): Unit = {
    parser.parse(parser.prog, reader) match {
      case parser.Success(term, nextReader) =>
        interpret(nextReader, ctx)
        interpret(term)
      case parser.Failure(msg, _) =>
        println(s"Parser error: $msg")
      case parser.Error(msg, _) =>
        println(s"Parser error: $msg")
    }
  }

  /**
   * Teste si t est un terme clos.
   * Si la réponse est négative, on formate le terme que l'on affiche sur la
   *  sortie standard et on signale qu'il est non clos.
   * Sinon, on essaie de le typer, puis de l'évaluer et de retourner la valeur
   *  obtenue avec la méthode "typingAndEval".
   */
  private def interpret(t: Term): Unit = {
    if (!Util.isClosed(t)) {
      println(s"Term $t is not closed.")
      println(s"Formatted term: ${formatter.format(t)}")
    } else {
      typingAndEval(t)
    }
  }

  /**
   * Essaie de typer t.
   * En cas de succès, on évalue t et affiche la valeur obtenue après formatage
   *  ainsi que son type. La valeur est ensuite retournée.
   * Sinon, on signale l'erreur de typage.
   */
  private def typingAndEval(t: Term): Unit = {
    typeChecker.typeOf(t) match {
      case Right(typ) =>
        val result = evaluator.eval(t)
        println(s"Formatted result: ${formatter.format(result)}")
        println(s"Type of result: $typ")
      case Left(error) =>
        println(s"Type error: $error")
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val reader = StreamReader(new FileReader("src/test/scala/tp07/examples.txt"))
    new Interpretor().interpret(reader)
  }
}