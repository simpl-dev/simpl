// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

import GrammarUtils._

/** Represents class or trait that is generated from a grammar rule.
  * @param name Name of the class.
  * @param classType This will be either "trait" or "case class", depending
  * on the type of the rule.
  * @param body Scala code from the grammar rule that will be inserted
  * to the generated Scala class.
  */
class RuleClass(val name: String, val classType: String, body: String) {
    /** Set of classes that this class will extend. */
    val extend = new collection.mutable.HashSet[String]

    /** Constructor parameters. */
    val params = new ArrayBuffer[RuleClassParam]

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
            buf.append(stripQuotes(body))
        }
        buf.append("}")
    }

    /** Generates code for the class. */
    def generate(buf: StringBuilder) {
        // Class header
        buf.append(toString)

        // Class body.
        bodyCode(buf)
        buf.append("\n")
    }

    /** Does the class need to have parameter list?
      * In Scala, traits without parameters are OK, but case classes need
      * parameter list, even if it is empty. */
    private def hasParamList = classType != "trait"
}

/** Constructor parameter for rule class. */
class RuleClassParam(val name: String, val pType: String) {
    override def toString = name + ": " + pType
}
