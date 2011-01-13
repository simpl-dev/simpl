// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas

import org.antlr.runtime._
import java.io.InputStream
import java.io.Reader
import collection.mutable.ArrayBuffer

/** Provides access to DSL grammar. In general, all the grammar-related
  * classes are automatically generated. */
abstract class GenericGrammar {
    /** Abstract syntax tree of the DSL program. Created by one of the
      * parse methods. */
    def tree: CommonNode
    /** Returns true, if the grammar object contains a parse tree.
      * The tree can be missing when parse method is not called yet or
      * when the parsing failed completely (partially failed parsing
      * produces partial parse tree). */
    def hasTree: Boolean

    /** Returns list of tokens in the DSL program. */
    def tokens: IndexedSeq[GenericToken]
    /** Returns list of errors encountered during parsing. */
    def errors: IndexedSeq[SourceMessage]
    /** Returns all the known keywords in this language. */
    def keywords: Seq[String]

    /** Read DSL program from file and parse to syntax tree. */
    def parseFile(filename: String) =
        parse(new ANTLRFileStream(filename))

    /** Read DSL program from string and parse to syntax tree. */
    def parseString(source: String) =
        parse(new ANTLRStringStream(source))

    /** Read DSL program and parse to syntax tree. */
    def parse(source: InputStream): Unit =
        parse(new ANTLRInputStream(source))

    /** Read DSL program and parse to syntax tree. */
    def parse(source: Reader): Unit =
        parse(new ANTLRReaderStream(source))

    /** Read DSL program and parse to syntax tree. */
    def parse(source: CharStream): Unit
}
