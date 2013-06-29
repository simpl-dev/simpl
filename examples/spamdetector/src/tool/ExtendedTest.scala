package ee.cyber.simplicitas.spamdetector.extended

import ee.cyber.simplicitas.{PrettyPrint, MainBase}

object ExtendedMain extends MainBase {
    def main(argv: Array[String]) {
        parseOptions(argv)
        val grammar = new SDExtendedGrammar()
        for (arg <- sources) {
            grammar.parseFile(arg)
            checkErrors(grammar.errors)

            println(PrettyPrint.prettyPrint(grammar.tree))
        }
    }
}
