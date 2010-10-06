// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

/** Information about other rule that is called from current rule.
  * @param name ???
  * @param ruleName name of the rule that will be called
  * @param varName Name of the variable used in the code.
  * @param listVar name of the temporary variable used to collect
  * the list elements.
  * @param branch identifies alternative branch where this rule call belongs to.
  * Branches are used to detect conflicting variable names.
  */
case class RuleParam(name: String, ruleName: String, var varName: String,
                     listVar: String, branch: BranchIdentifier) {
    /** whether the result of the rule call is list (use of + or *). */
    def isList = listVar ne null
}

object BranchIdentifier {
    val empty = new BranchIdentifier(List(0))
}

class BranchIdentifier(branch: List[Int]) {
    def nextBranch = 
        new BranchIdentifier((branch dropRight 1) ++ List(branch.last + 1))
    def extend = new BranchIdentifier(branch ++ List(0))

    override def toString = branch.toString
}

/** Code for generating code for a single grammar rule. */
class RuleGen(symbols: SymbolTable, g: ArrayBuffer[String], 
        posMap: Any => List[Int]) {
    import symbols._
    import GrammarUtils._
    
    val error = GrammarUtils.error(posMap)_

    /** Identifies current branch in the options. */
    var currentBranch: BranchIdentifier = BranchIdentifier.empty

    /** whether the currently analyzed branch contains
      * repetition (Foo+ or Foo*). */
    var isList = false

    /** Collection of rules called from this rule. */
    private val params = new ArrayBuffer[RuleParam]()

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
        matchAltList(matchTerminalAction, matchCodeBlock(alt, ruleName))
        if (isHidden) {
            g += "{$channel = HIDDEN;}"
        }
        g += ";\n"
    }

    /** Generates code for standard rule in the form:
      * Foo: Bar Baz | Bab;
      */
    def generateNormalRule(name: String, alt: List[Any]) {
        println("normal rule: " + name + ": " + alt)

        g += "\n" + rules(name).antlrName + " returns [" + name +
            " r]\n@init {SourceLocation _start=null;int _end=-1;" +
            "int _endLine=-1;int _endColumn=-1;"

        // The following string will be modified later to include
        // initialization of temporary variables. These variables will
        // be determined by call to altList.
        g += ""
        val init_idx = g.size - 1

        g += "}\n@after {$r = new " + name + "("

        // The following string will be modified later to include 
        // post-processing.
        g += ""
        val after_idx = g.size - 1

        g += ");$r.setLocation(_start,_end==-1?(_start==null?0:_start.endIndex()):_end," +
            "_endLine==-1?(_start==null?0:_start.endLine()):_endLine," +
            "_endColumn==-1?(_start==null?0:_start.endColumn()):_endColumn);}:\n"

        matchAltList(matchName, matchCodeBlock(alt, name))

        g += ";\n"
        val init = new StringBuilder()
        def getParam(p: RuleParam): String = {
            println("getParam(" + p + ")")
            if (p.isList) {
                init append ("ArrayList " + p.listVar + "=new ArrayList();")
                "scalaList(" + p.listVar + ")"
            } else {
                return nodeValue(p)
            }
        }

        g(after_idx) = join(params.map(getParam))
        g(init_idx) = init.toString
        rules(name).classType = "case class " + name
        rules(name).parameters = 
            for (p <- params) yield
                ConstructorParam(p.name, 
                        if (p.isList) "List[" + p.ruleName + "]" 
                        else p.ruleName,
                        "", "var ")
        for (p <- params) {
            if (!(rules contains p.ruleName)) {
                error(p.ruleName, "Undefined rule " + p.ruleName
                        + " referenced")
            }
        }
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
            val np = RuleParam(id, option, id, null, BranchIdentifier.empty)
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

    /** Processes list of alternatives. The tree contains list of alternatives,
      * each wrapped in a list starting with NODE.
      * @param doMatch function that will be called for each alternative. 
      * @param tree AST containing list of alternatives for a rule.
      */
    def matchAltList(doMatch: List[Any] => List[Any], tree: List[Any]) {
        var first = true
        for ("NODE" :: matches <- tree) {
            if (!first) {
                g += "|"
                currentBranch = currentBranch.nextBranch
            }
            var i = matches
            while (!i.isEmpty) {
                i = doMatch(i)
            }
            first = false
        }
    }

    /** Checks whether the terminal pattern contains a code block
      * and generates code for it.
      * TODO: does this feature have any use? */
    def matchTerminalAction(tree: List[Any]): List[Any] = tree match {
        case (s: String) :: t if s startsWith "{" =>
            val r = matchModifier(matchTerminalPattern, t)
            g += s
            r
        case _ =>
            matchModifier(matchTerminalPattern, tree)
    }

    /** Check whether current element will comes with modifier, such as
      * ?, + or *. If so, do some bookkeeping to checking whether
      * two variable names conflict or not. In any case, call the
      * argument <code>f</code> with the current element as argument. */
    def matchModifier(f: List[Any] => List[Any], tree: List[Any]): List[Any] = {
        /** Calls f(tree) in the context where isList = true. */
        def callWithList(tree: List[Any], modifier: String): List[Any] = {
            // Store the previous value.
            val oldIsList = isList

            // Call the function with isList set to true.
            isList = true
            val result = f(tree)
            isList = oldIsList

            // Add the modifier itself to output.
            g += modifier
            result
        }

        tree match {
            case "?" :: t =>
                val ret = f(t)
                g += "?"
                ret
            case "*" :: t =>
                callWithList(t, "*")
            case "+" :: t =>
                callWithList(t, "+")
            case t =>
                f(t)
        }
    }

    /** Processes the unnamed rule call. */
    val simple_null = simple(null)_

    /** Takes as input list of terms. If the list starts with
      * (= ...) element, then assumes that this will become the name of
      * the next term. Otherwise, the name will be null. */
    def matchName(tree: List[Any]): List[Any]  = tree match {
        case List("=", name: String) :: t => 
            matchModifier(simple(name), t)
        case t => 
            matchModifier(simple_null, t)
    }

    def simple(name: String)(tree: List[Any]): List[Any] = tree match {
//        case (id: String) :: t =>
//            simpleTerm(name, id)
//            firstInChain = false
//            t
//        case ("(" :: alt) :: t =>
//            if (name ne null)
//                error(name, "Complex pattern cannot be given a name")
//            g += "("
//            currentOption = currentOption ++ List(0)
//            altList(matchName, alt)
//            currentOption = currentOption dropRight 1
//            g += ")"
//            firstInChain = false
//            t
        case Nil =>
            Nil
        case _ => Nil
    }

    /** Matches and generates code for terminal pattern. */
    def matchTerminalPattern(tree: List[Any]): List[Any] = tree match {
        case h :: t => h match {
            case s: String => s match {
                // ~Foo
                case "~" =>
                    g += " ~"
                    matchTerminalPattern(t)
                // just identifier Foo
                case _ =>
                    g += " "
                    g += s
                    t
            }
            case l: List[Any] => l match {
                // ( Foo )
                case "(" :: alt =>
                    g += "("
                    matchAltList(matchTerminalAction, alt)
                    g += ")"
                    t
                // Foo .. Bar
                case ".." :: (from: String) :: (to: String) :: Nil =>
                    g += " " + from + ".." + to
                    t
                // Just to satisfy the compiler.
                case _ =>
                    throw new IllegalArgumentException()
            }
        }
        case Nil => Nil
    }
}
