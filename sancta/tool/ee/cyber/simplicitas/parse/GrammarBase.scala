// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import org.antlr.runtime.{CharStream, TokenSource, TokenStream,
                          CommonTokenStream, RecognitionException}
import collection.mutable.ArrayBuffer
import ee.cyber.simplicitas._

/** Base class for grammar that is parameterized by the node type and
  * list of token kinds. */
abstract class GrammarBase[Node <: CommonNode, Kind]
        extends GenericGrammar with TokenClassifier[Kind] {
    private var tokenList: Array[CommonToken[Kind]] = Array()
    private var nodeTree: Option[Node] = None
    protected var errorHandler: ErrorHandler = null
    var grammarFile: String = null

    // override in generated concrete grammar
    protected def lexer(source: CharStream,
                        errorHandler: ErrorHandler): TokenSource
    protected def doParse(tokens: TokenStream,
                          errorHandler: ErrorHandler): Node
    protected def tokenNames: Map[Int, String]

    def tree: Node = nodeTree.get
    def hasTree = nodeTree != None

    def tokens: IndexedSeq[CommonToken[Kind]] = tokenList
    def errors: IndexedSeq[SourceMessage] = errorHandler.errors

    def parse(source: CharStream, filename: String) {
        grammarFile = filename
        errorHandler = new ErrorHandler(tokenNames)
        val tokenSource =
            new TokenSourceWrapper[Kind](lexer(source, errorHandler), this)
        val tokenStream = new CommonTokenStream(tokenSource)

        try {
            val tree = doParse(tokenStream, errorHandler)

            if (tree ne null) {
                for (child <- tree.children if child ne null)
                    child.updateParents(tree)
                nodeTree = Some(tree)
            } else {
                nodeTree = None
            }
        } catch {
            case pe: ParseError =>
                errorHandler.errors += pe.sourceMessage
        }

        tokenList = tokenSource.tokens
    }
}

/** This machinery is needed for reporting parse errors.
    It is not part of the public API. */
class ErrorHandler(val tokenNames: Map[Int, String]) {
    val errors = new ArrayBuffer[SourceMessage]()
    var lastError: RecognitionException = null

    def emitErrorMessage(message: String) {
        if (lastError.token ne null) {
            val t = lastError.token.asInstanceOf[
                org.antlr.runtime.CommonToken]
            errors += new SourceMessage(message, SourceMessage.Error,
                                    t.getStartIndex, t.getStopIndex,
                                    t.getLine, t.getCharPositionInLine)
        } else {
            // Lexer error, we'll assume that the problem was
            // a single character.
            val start = lastError.index
            val stop = start + 1
            errors += new SourceMessage(message, SourceMessage.Error,
                                        start, stop,
                                        lastError.line,
                                        lastError.charPositionInLine)
        }
    }

    def reportError(ex: RecognitionException) {
        lastError = ex
    }
}
