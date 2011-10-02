// Copyright (c) 2010-2011 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import ee.cyber.simplicitas.{SourceMessage, SourceLocation}

class ParseError(location: SourceLocation, message: String) extends Exception {
    def sourceMessage =
        new SourceMessage(message, SourceMessage.Error, location)
}