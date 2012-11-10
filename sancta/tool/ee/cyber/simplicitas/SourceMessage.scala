// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas

/** Source message message kind values. */
object SourceMessage extends Enumeration {
    val Error = Value("Error")
    val Warning = Value("Warning")
    val Info = Value("Info")

    def error(message: String, location: SourceLocation) =
        new SourceMessage(message, Error, location)
    def warning(message: String, location: SourceLocation) =
        new SourceMessage(message, Warning, location)
    def info(message: String, location: SourceLocation) =
        new SourceMessage(message, Info, location)
}

/** Message about the DSL program. It contains human-readable text
  * and location in the source file. */
class SourceMessage(val message: String, val kind: SourceMessage.Value,
                    val startOffset: Int, val endOffset: Int,
                    val startLine: Int, val startColumn: Int) {
    def this(message: String, kind: SourceMessage.Value,
             location: SourceLocation) {
        this(message, kind, location.startIndex, location.endIndex,
             location.startLine, location.startColumn)
    }

    override def toString() =
        kind + " (" + startLine + ":" + startColumn + "): " + message
}
