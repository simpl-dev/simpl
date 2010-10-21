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

    /** Return type of the rule. Can be different from Scala class
      * because of wrapper rules and the fact that ANTLR does not support
      * parameterized types. Often, this can be a lazy value and, therefore,
      * you should use toString method to get actual value. */
    var returnType: Any = ""

    private val extendBuf = new ArrayBuffer[String]()

    /** What classes will we extend? */
    def extendWith(name: String) {
        if (!(extendBuf contains name)) {
            extendBuf += name
        }
    }

    def extendList: List[String] = {
        val wrappedExtends = wrappedFrom.flatMap(_.extendList)
        val withoutDuplicates = (extendBuf ++ wrappedExtends).toSet
        // Remove myself from extends list to prevent construct
        // class Foo extends Foo
        (withoutDuplicates - antlrName).toList
    }

    /** Class body, if user uses the { ... } construct. */
    var body = ""

    /** Should we generate class from this rule? */
    var generateCode = true

    /** List if wrapper rules that call this rule. */
    val wrappedFrom = new ArrayBuffer[RuleClass]()

    def addWrappedFrom(rule: RuleClass) {
        if (!(wrappedFrom contains rule)) {
            println(antlrName + ".addWrappedFrom(" + rule.antlrName + ")")
            wrappedFrom += rule
        }
        // Make the wrapper point to us.
        rule.wrappedRule = this
    }

    var wrappedRule: RuleClass = null

    def withoutCodegen = {
        generateCode = false
        this
    }
}

object RuleClass {
    /** Creates RuleClass object corresponding to terminal rule with
      * given name. */
    def terminalRule(name: String) = {
        val termClass = new RuleClass(name)
        termClass.extendWith("TerminalNode")
        termClass.classType = "case class " + name
        termClass.returnType = name
        termClass.parameters =
            List(ConstructorParam("text", "String", ""))

        termClass
    }
}

/** Represents constructor parameter for Scala class representing this rule.
  * @param name name of the parameter
  * @param vtype type of the parameter. Can be delayed object which means
  * that you must call toString method to get the actual type value.
  * @param mod additional modifier for parameters, such as "var"
  */
case class ConstructorParam(name: String, vtype: Any, mod: String)

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

/** Cells whose contents can be changed later. */
class MutableCell[T](init: T) {
    private var elem: T =  init

    /** Set new value for cell. */
    def set(newVal: T) {
        elem = newVal
    }
    
    /** Get the contents. */
    def apply() = elem

    override def toString = String.valueOf(elem)
}

/** Constructor for mutable strings. */
object LazyString {
    /** Create empty string */
    def apply() = new MutableCell[String]("")

    /** Create string with value. */
    def apply(initVal: String) = new MutableCell[String](initVal)
}

class Delayed[T](block: => T) {
    private lazy val value: T = block

    def apply() = value
    override def toString = String.valueOf(value)
}

object Delayed {
    def apply(block: => String) = new Delayed(block)
}