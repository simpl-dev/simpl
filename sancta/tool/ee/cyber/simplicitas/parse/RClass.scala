package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

class RClass(val name: String, val classType: String, body: String) {
    val extend = new collection.mutable.HashSet[String]
    val params = new ArrayBuffer[RCParam]

    override def toString =
        classType + " " + name + paramsCode + " " + extendsCode

    private def paramsCode = 
        if (hasParamList)
            "(" + params.mkString(", ") + ")"
        else 
            ""

    private def extendsCode =
        if (extend.isEmpty)
            "extends CommonNode"
        else
            "extends " + extend.mkString(" with ")

    private def bodyCode(buf: StringBuilder) {
        if (!hasParamList && (body eq null)) {
            // Avoid empty brackets.
            return
        }

        buf.append(" {\n")
        if (hasParamList) {
            buf.append("    def childrenNames = Array(")
            buf.append(params.map("\"" + _.name + "\"").mkString(", "));
            buf.append(");\n")
        }
        if (body ne null) {
            // Strip the {} marks from beginning and the end.
            buf.append(body.substring(1, body.length - 1))
        }
        buf.append("}")
    }

    def generate(buf: StringBuilder) {
        buf.append(toString)

        bodyCode(buf)
        buf.append("\n")
    }

    private def hasParamList = classType != "trait"
}

class RCParam(val name: String, val pType: String) {
    override def toString = name + ": " + pType
}

