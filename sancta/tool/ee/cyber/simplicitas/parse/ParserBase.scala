// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import collection.mutable.ArrayBuffer
import ee.cyber.simplicitas.CommonNode
import org.antlr.runtime.{Token, TokenStream, RecognizerSharedState,
                          RecognitionException}

/** Base class for generated ANTLR parsers. */
class ParserBase(input: TokenStream, state: RecognizerSharedState)
        extends org.antlr.runtime.Parser(input, state) {
    def setTokenPos(node: CommonNode, token: Token): CommonNode = {
        val t = new TokenLocation(token)
        node.startIndex = t.startIndex
        node.endIndex = t.endIndex
        node.startLine = t.startLine
        node.startColumn = t.startColumn
        node.endLine = t.endLine
        node.endColumn = t.endColumn
        node
    }

    def scalaList[X](l: java.util.ArrayList[X]): List[X] = {
        var res: List[X] = Nil
        for (i <- l.size - 1 to 0 by -1)
            res = (l get i) :: res
        res
    }

    // This machinery is needed for reporting parse errors.
    var errorHandler: ErrorHandler = null

    override def emitErrorMessage(message: String) {
      errorHandler.emitErrorMessage(message)
    }

    override def reportError(ex: RecognitionException) {
        errorHandler.reportError(ex)
        super.reportError(ex)
    }

    // The default implementation method includes the line and column
    // information in the string error message. We modify that to
    // only include verbatim error message.
    override def displayRecognitionError(tokenNames: Array[String],
                                         ex: RecognitionException) {
        emitErrorMessage(getErrorMessage(ex, tokenNames))
    }
}
