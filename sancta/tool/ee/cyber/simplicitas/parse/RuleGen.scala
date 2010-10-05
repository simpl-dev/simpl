// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

/** Information about other rule that is called from current rule.
  * Constructor parameters:
  * - name ??
  * - node -- type of the node.
  * - var ??
  * - isList -- whether the result of the rule call is list (use of + or *).
  * - tmp ??
  * - option ??
  */
case class NodeParam(name: String, node: String, var varName: String,
                     isList: Boolean, tmp: String, option: List[Int])

/** Code for generating code for a single grammar rule. */
class RuleGen(symbols: SymbolTable, g: ArrayBuffer[String], 
        posMap: Any => List[Int]) {
    import symbols._
    import GrammarUtils._
    
    val error = GrammarUtils.error(posMap)_

    /** Generates code for rule given as AST. */
    def generate(tree: Any) {
        println("generate: " + tree)
        tree match {
            case "terminal" :: "hidden" :: (name: String) :: alt =>
                generateTerminal(name, alt, false, true)
            case "terminal" :: (name: String) :: alt =>
                generateTerminal(name, alt, false, false)
            case "fragment" :: (name: String) :: alt =>
                generateTerminal(name, alt, true, false)
            case ":" :: (name: String) :: alt =>
                generateNormalRule(name, alt)
            case "option" :: (name: String) :: alt =>
                generateOptionRule(name, alt)
            case _ =>
                ()
        }
    }

    /** Generates code for fragment or terminal rule. */
    def generateTerminal(ruleName: String, alt: List[Any], isFragment: Boolean,
            isHidden: Boolean) {
        if (isFragment) {
            g += "fragment "
        }

        g += ruleName + ':'
//        altList(termAction, matchCodeBlock(alt, ruleName))
        if (isHidden) {
            g += "{$channel = HIDDEN;}"
        }
        g += ";\n"
    }

    /** Generates code for standard rule in the form:
      * Foo: Bar Baz | Bab;
      */
    def generateNormalRule(ruleName: String, alt: List[Any]) {
    }

    /** Generates code for option rule:
      * option Foo: Bar | Baz;
      */
    def generateOptionRule(name: String, alt: List[Any]) {
        println("option rule " + name + ": " + alt)

        g += "\n" + rules(name).antlrName + " returns [" + name + " r]:\n"

        val optionList = matchCodeBlock(alt, name)
        var first = true

        for (t <- optionList) {
            // Assuming here that option rule contains just list of options.
            val option = t.toString

            if (!first) {
                g += " | "
            }

            if (!(rules.contains(option))) {
                error(t, "Undefined rule " + option + " referenced")
            }

            // Make the rule called from this option extend class
            // corresponding to this rule.
            val r = rules(option)
            if (!(r.extend contains name)) {
                r.extend += name
            }

            println("t(" + option + ")")
            val id = newId
            val np = NodeParam(id, option, id, false, null, Nil)
//            g += id + "=" + r.antlrName + "{$r=" + nodeValue(np) + ";}"
            first = false
        }
        g += ";\n"
    }

    /** Matches code block at the beginning of the rule. 
      * @param tree AST corresponding to rule.
      * @param ruleName name of the rule.
      * @return the rule body without the code block.
      */
    def matchCodeBlock(tree: List[Any], ruleName: String): List[Any] =
        tree match {
            case List("BODY", code: String) :: rest =>
                rules(ruleName).body = "\n" + code.substring(1, code.length - 1)
                rest
            case _ =>
                tree
        }
}
