// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.ide

import ee.cyber.simplicitas._

import java.io.File

object SimplMain extends MainBase {
    def main(argv: Array[String]) {
        parseOptions(argv)

        val grammar = new SimplGrammar()
        val generator = new DocGenerator(destDir)

        for (arg <- sources) {
            grammar.parseFile(arg)
            checkErrors(grammar.errors)

            val ctx = new SimplCtx(grammar)
            new SimplPostProcess(ctx).validate()
            checkErrors(ctx.errors)

            generator.generate(grammar.tree, baseName(arg))
        }
    }

    /** Returns basename of the file with extension changed from
      * .spl to .rst, for example:
      * /foo/bar/baz.spl -> baz.rst */
    def baseName(file: String) = {
        val withExtension =
            if (file.endsWith(".spl"))
                file.substring(0, file.length - 4)
            else
                file

        new File(withExtension).getName
    }
}
