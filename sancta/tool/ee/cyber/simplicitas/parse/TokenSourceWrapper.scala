// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import org.antlr.runtime.Token
import org.antlr.runtime.TokenSource
import scala.collection.mutable.ArrayBuffer
import ee.cyber.simplicitas.CommonToken

trait TokenClassifierConstants {
    final val normal   = 0
    final val keyword  = 1
    final val operator = 2
    final val hidden   = 3
    final val comment  = 4
}

trait TokenClassifier[Type] extends TokenClassifierConstants {
    def tokenKind(token: Int): Int
    def tokenType(token: Int): Type
}

class TokenImpl[Type](tokenText: String, enum: Type, tkind: Int)
        extends CommonToken[Type] with TokenClassifierConstants {
    def text       = tokenText
    def isKeyword  = tkind == keyword
    def isHidden   = tkind == hidden || tkind == comment
    def isOperator = tkind == operator
    def isComment  = tkind == comment
    def kind       = enum
    var startIndex = 0
    var endIndex   = 0
    var startLine  = 0
    var endLine    = 0
    var startColumn = 0
    var endColumn  = 0
    
    override def toString: String = "(" + kind + ")" + text
}

class TokenSourceWrapper[Type](ts: TokenSource, classifier: TokenClassifier[Type])
        extends TokenSource {
    private val tokenArr = {
        val buf = new ArrayBuffer[Token]()
        var token: Token = null
        do {
            token = ts.nextToken
            buf += token
        } while (token.getType() != -1);
        buf toArray
    }
    val tokens: Array[CommonToken[Type]] =
        for (token <- tokenArr if token.getType != Token.EOF) yield {
            val t = token.asInstanceOf[org.antlr.runtime.CommonToken]
            val typeId = t.getType
            val kind = if (t.getChannel == Token.HIDDEN_CHANNEL)
                           classifier.comment
                       else classifier tokenKind typeId
            val ct =
                new TokenImpl(t getText, classifier tokenType typeId, kind)
            val tl = new TokenLocation(t)
            ct.startIndex = tl.startIndex
            ct.endIndex = tl.endIndex
            ct.startLine = tl.startLine
            ct.startColumn = tl.startColumn
            ct.endLine = tl.endLine
            ct.endColumn = tl.endColumn
            ct
        }
    private var index = -1
    private val eof = new org.antlr.runtime.CommonToken(-1)

    override def nextToken(): Token = {
        index += 1
        if (index >= tokenArr.length) eof else tokenArr(index)
    }

    override def getSourceName(): String =
        ts.getSourceName
}
