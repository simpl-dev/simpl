// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

/** Information about other rule that is called from current rule.
  * @param name ???
  * @param ruleName name of the rule that will be called
  * @param varName Name of the variable used in the code.
  * @param isList whether the result of the rule call is list (use of + or *).
  * @param branch identifies alternative branch where this rule call belongs to.
  * Branches are used to detect conflicting variable names.
  */
case class RuleParam(name: String, ruleName: String, var varName: String,
                     isList: Boolean, branch: BranchIdentifier)

object BranchIdentifier {
    val empty = new BranchIdentifier(List(0))
}

class BranchIdentifier(branch: List[Int]) {
    def nextBranch = 
        new BranchIdentifier((branch dropRight 1) ++ List(branch.last + 1))
    def extend = new BranchIdentifier(branch ++ List(0))
}

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
            val np = RuleParam(id, option, id, false, BranchIdentifier.empty)
            g += id + "=" + r.antlrName + "{$r=" + nodeValue(np) + ";}"
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

    /** Returns code for getting the value of given node. */
    def nodeValue(p: RuleParam) = {
        val name = "$" + p.varName

        if (terminals contains p.ruleName) {
            val v = "(" + p.ruleName + ")setTokenPos(new " + p.ruleName + "(" +
                name + ".getText()" + ")," + name + ")"

            name + "==null?null:" + v
        } else {
            name + ".r"
        }
    }
}
