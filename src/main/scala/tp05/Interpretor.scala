package tp05

import java.io.FileReader
import scala.util.parsing.input.StreamReader
import scala.util.parsing.input.Reader

class Interpretor {
  val parser = new Parser
  val evaluator = new Evaluator

  def interpret(reader: Reader[Char]): Unit = {
    parser.parseAll(parser.term, reader) match {
      case parser.Success(term, _) =>
        val result = evaluator.evaluate(term)
        println(s"Résultat de l'interprétation : $result")
      case parser.Failure(msg, _) =>
        println(s"Erreur de syntaxe : $msg")
      case parser.Error(msg, _) =>
        println(s"Erreur d'analyse : $msg")
    }
  }
}


object Main {
  def main(args: Array[String]) = {
    val reader = StreamReader(new FileReader("src/test/scala/tp05/examples.txt"))
    new Interpretor().interpret(reader)
  }
}