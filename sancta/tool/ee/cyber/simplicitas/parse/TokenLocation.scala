// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import org.antlr.runtime.Token
import ee.cyber.simplicitas.SourceLocation

class TokenLocation(token: Token) extends SourceLocation {
    private val t = token.asInstanceOf[org.antlr.runtime.CommonToken]

    def startIndex: Int = t.getStartIndex
    def endIndex: Int = t.getStopIndex
    def startLine: Int = t.getLine
    def endLine: Int = t.getLine + (t.getText filter (_=='\n') size)
    def startColumn: Int = t.getCharPositionInLine + 1
    def endColumn: Int = {
        val s = t.getText
        val i = s.lastIndexOf("\n")

        if (i == -1) startColumn + s.length - 1
        else s.substring(i + 1).length
    }
}
