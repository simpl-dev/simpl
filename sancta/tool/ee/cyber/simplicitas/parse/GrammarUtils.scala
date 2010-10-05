// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer


/** Contains the symbols that are found in the grammar. */
trait SymbolTable {
    /** Set of used terminal identifiers. */
    def terminals: collection.mutable.Set[String]

    /** Keywords supported by this language. Map is from keyword
      * to identifier representing the corresponding lexer rule. */
    def keywords: collection.mutable.Map[String, String]

    /** Rules in this language, indexed by rule name. */
    def rules: collection.mutable.Map[String, RuleClass]
    
    private var idcounter = 0

    def newId = {
        idcounter += 1
        println("newId(" + "Z" + idcounter + ")")
        "Z" + idcounter
    }
}

/** Represents class that is generated for each rule. */
class RuleClass(val antlrName: String) {
    /** This will be either "trait" or "case class" depending on
      * the type of the rule. */
    var classType = ""

    /** Constructor parameters. */
    var parameters: Seq[ConstructorParam] = Nil

    /** What classes will we extend? */
    val extend = new ArrayBuffer[String]()

    /** Class body, if user uses the { ... } construct. */
    var body = ""
}

case class ConstructorParam(name: String, vtype: String, code: String,
        mod: String)

class GrammarException(msg: String) extends Exception(msg)



/** Various utility methods useful for grammar generation. */
object GrammarUtils {
    /** Report an error related to node. */
    def error(posMap: Any => List[Int])(node: Any, what: String) =
        throw new GrammarException(posMap(node) match {
                case List(line, col) =>
                    line + ":" + col + ": " + what
                case _ =>
                    what
            })

    /** Make the first character of the string lowercase. */
    def uncapitalize(s: String): String =
        if (s == "")
            ""
        else
            (Character toLowerCase (s charAt 0)) + (s substring 1)

    def join(i: Iterable[Any]) = i.mkString(", ")
}