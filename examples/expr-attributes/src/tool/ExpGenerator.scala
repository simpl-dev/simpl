package ee.cyber.simplicitas.expattributes

import ee.cyber.simplicitas.MainBase

object ExpMain extends MainBase {
  def main(argv: Array[String]) {
    val grammar = new ExpGrammar()

      for (s <- argv) {
          println("\nProcessing: " + s)
          grammar.parseString(s)
          checkErrors(grammar.errors)
          val ret = Evaluator.evaluate(grammar.tree)
          println("Result: " + ret);
      }
  }
}
