package ee.cyber.simplicitas.grammartest

import ee.cyber.simplicitas.CommonNode

object Dump {
    val indent = "  "

    def dumpNode(node: CommonNode): String = {
        val buf = new StringBuilder()
        dumpNode(buf, node, "")
        buf.toString
    }

    def dumpNode(buf: StringBuilder, node: CommonNode, 
            prefix: String) {
        if (node == null)
            return

        // Write node header.
        buf.append(prefix)
        buf.append(node.startIndex)
        buf.append(":")
        buf.append(node.endIndex)
        buf.append(":")
        buf.append(baseName(node.getClass.getName))
        buf.append("\n")

        // Write children
    }

    def baseName(s: String) = {
        val idx = s.lastIndexOf('.')
        if (idx == -1)
            s
        else
            s.substring(idx + 1)
    }
}