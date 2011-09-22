package ee.cyber.simplicitas

import prettyprint.Doc
import Doc._

/** Pretty-prints the AST node. */
object PrettyPrint {
    def prettyPrint(node: CommonNode): String = {
        val doc = format(node)
        doc.toString
    }

    private def format(obj: Any): Doc = obj match {
        case lst: List[Any] =>
            brackets(docToDoc(lst.map(format)))
        case node: Product =>
            formatCommonNode(node)
        case null =>
            text("null")
        case _ =>
            text("\"" + obj.toString + "\"")
    }

    private def docToDoc(lst: List[Doc]) =
        align(withCommas(lst))

    private def formatCommonNode(node: Product): Doc =
        baseName(node.getClass.getName) ::
                parens(
                    docToDoc(getChildren(node).map(format)))

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