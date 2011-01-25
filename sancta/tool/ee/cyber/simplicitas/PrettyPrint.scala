package ee.cyber.simplicitas

import scala.text.Document
import Document._

/** Pretty-prints the AST node. */
object PrettyPrint {
    private val indent = "  "

    def prettyPrint(node: CommonNode): String = {
        val doc = format(node)
        val writer = new java.io.StringWriter
        doc.format(60, writer)
        writer.flush()
        writer.toString
    }

    private def format(obj: Any): Document = obj match {
        case lst: List[Any] =>
            group(
                text("[") :/:
                        docToDoc(lst.map(format).map(nest(2, _))) :/:
                        text("]"))
        case node: Product =>
            formatCommonNode(node)
        case null =>
            text("null")
        case _ =>
            text("\"" + obj.toString + "\"")
    }

    private def docToDoc(lst: Seq[Document]) =
        lst.reduceRightOption(_ :: "," :/: _).getOrElse(Document.empty)

    private def formatCommonNode(node: Product): Document = {
        group(
            text(baseName(node.getClass.getName) + "(") :/:
                    docToDoc(getChildren(node).map(format).map(nest(2, _))) :/:
                    text(")"))
    }

    private def getChildren(node: Product) =
        node.productIterator.toList

    private def baseName(s: String) = {
        val idx = s.lastIndexOf('.')
        if (idx == -1)
            s
        else
            s.substring(idx + 1)
    }
}