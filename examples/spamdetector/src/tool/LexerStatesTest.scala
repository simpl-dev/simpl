package ee.cyber.simplicitas.spamdetector.lexerstates

import ee.cyber.simplicitas.{PrettyPrint, MainBase}

object LexerStatesMain extends MainBase {
    def main(argv: Array[String]) {
        parseOptions(argv)
        val grammar = new LSGrammar()
        for (arg <- sources) {
            grammar.parseFile(arg)
            checkErrors(grammar.errors)

            println(PrettyPrint.prettyPrint(grammar.tree))
        }
    }
}
