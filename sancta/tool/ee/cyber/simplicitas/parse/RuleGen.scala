// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

/* The code in this file is responsible for generating Scala class and
 * ANTLR rules for a single rule. Some notes about how it is organized.
 *
 * There are many methods in the style of:
 * def matchFoo(tree: List[Any], ...): List[Any]
 *
 * In general, these methods take as input AST disguised as list.
 * They try to find Foo from the beginning of the list, do some actions
 * based on found data, and return the rest of the list (i.e., everything
 * after Foo).
 *
 *
 * The code tries to ensure that there are no name clashes for rule calls
 * in different branches. For example, this is legal rule:
 * Foo: x=Bar y=Baz | x=Bar z=Bag;
 *
 * Although the variable x is present in both options, it has the same type
 * and can therefore safely be used. This is incorrect:
 * Foo: x=Bar y=Baz | x=Baz z=Bag;
 *
 * Now x is type Bar in one branch and Baz in another.
 *
 * The important point is to determine which uses of variable would clash
 * (variable is used two times in the same branch). For example, consider
 * the following rule (for brevity, here assignments are not used for
 * sub-rule calls):
 *
 * Foo: A ((B C) | (D (E | F)?));
 *
 * In this case, A would clash with all the other rule calls, but B and E,
 * for example, do not clash. However, D and F clash. To determine, which
 * subrule calls clash, each branch in the rule is assigned an identifier.
 * If the same variable us used several times in a rule, then the identifiers
 * are compared to see whether the uses of this variable clash.
 *
 * Rule identifers are lists of integers that represents path to a sub-rule
 * call. For example, identifiers for the previous rule, are:
 * A: [0]
 * B: [0, 0]
 * C: [0, 0]
 * D: [0, 1]
 * E: [0, 1, 0]
 * F: [0, 1, 1]
 *
 * Two sub-rule calls with identifiers I1 and I2 clash if I1 is prefix of
 * I2 or vice versa.
 */

/** Information about other rule that is called from current rule.
  * @param name variable name for rule in the grammar ("foo" in foo=Bar).
  * @param ruleName name of the rule that will be called.
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

    /** What class will be used to represent this parameter in Scala code.
      * In general, this class is named after grammar rule. But for parameters
      * whose values are literal strings, we use LiteralNode class from
      * standard library.
      */
    var scalaClass = ruleName
}

object BranchIdentifier {
    /** The initial value for branch identifiers. */
    val empty = new BranchIdentifier(List(0))
}

/** Represents branch identifiers. See the comment at the beginning of the file
  * for details. */
class BranchIdentifier(val branch: List[Int]) {
    /** Returns identifier for the sibling branch. */
    def nextBranch = 
        new BranchIdentifier((branch dropRight 1) ++ List(branch.last + 1))

    /** Adds one level to the branch identifier. */
    def extend = new BranchIdentifier(branch ++ List(0))

    /** Returns true, if this branch conflicts with other branch. */
    def conflictsWith(other: BranchIdentifier) =
        (other.branch.zip(branch)) forall
                        ((a: Tuple2[Int, Int]) => a._1 == a._2)

    override def toString = branch.toString
}

/** Code for generating code for a single grammar rule.
  * @param symbols information about the rules in the grammar.
  * @param termCode buffer that will contain ANTLR code for terminal rules.
  * @param nonTermCode buffer that will contain ANTLR code for non-terminal
  * rules.
  * @param posMap function that will return node's position in the
  * input program.
  */
class RuleGen(symbols: SymbolTable, termCode: ArrayBuffer[String], 
        nonTermCode: ArrayBuffer[String], posMap: Any => List[Int]) {
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

    /** Temporary buffer that will point to either <code>termCode</code>
      * or <code>nonTermCode</code>. This allows using the same methods
      * to generate code for terminals and non-terminals. */
    private var g: ArrayBuffer[String] = null

    /** Generates code for rule given as AST. */
    def generate(tree: Any) {
        println("generate: " + tree)
        tree match {
            case "terminal" :: "hidden" :: (name: String) :: alt =>
                g = termCode
                generateTerminal(name, alt, false, true)
            case "terminal" :: (name: String) :: alt =>
                g = termCode
                generateTerminal(name, alt, false, false)
            case "fragment" :: (name: String) :: alt =>
                g = termCode
                generateTerminal(name, alt, true, false)
            case ":" :: (name: String) :: alt =>
                g = nonTermCode
                generateNormalRule(name, alt)
            case "option" :: (name: String) :: alt =>
                g = nonTermCode
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
        // code for all the constructor parameters for this node.
        g += ""
        val params_idx = g.size - 1

        g += ");$r.setLocation(_start,_end==-1?(_start==null?0:_start.endIndex()):_end," +
            "_endLine==-1?(_start==null?0:_start.endLine()):_endLine," +
            "_endColumn==-1?(_start==null?0:_start.endColumn()):_endColumn);}:"

        matchAltList(matchName, matchCodeBlock(alt, name))

        g += ";\n"

        // Now that we know all the constructor parameters for this
        // rule, fill in the temporary variables for list parameters.
        val init = new StringBuilder()
        for (p <- params if p.isList) {
            init.append("ArrayList " + p.listVar + "=new ArrayList();")
        }
        g(init_idx) = init.toString

        // Fill in the code for constructor parameters when creating
        // object to contain results of this rule.
        g(params_idx) = join(params.map(
                p => if (p.isList) {
                    "scalaList(" + p.listVar + ")"
                    } else {
                        nodeValue(p)
                    }))

        // Fill in the info needed for generating the Scala class.
        rules(name).classType = "case class " + name
        rules(name).parameters = 
            for (p <- params) yield
                ConstructorParam(p.name, 
                        if (p.isList) "List[" + p.scalaClass + "]" 
                        else p.scalaClass,
                        "", "var ")

        // Quick last check for invalid rule calls.
        for (p <- params) {
            if (!(rules.contains(p.ruleName))) {
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

        g += "\n" + rules(name).antlrName + " returns [" + name + " r]:\n    "

        val optionList = matchCodeBlock(alt, name)
        var first = true

        for (t <- optionList) {
            // Assuming here that option rule contains just list of options.
            val option = t.toString

            if (!first) {
                g += "\n    | "
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

    /** Returns code for getting the value of given rule call. */
    def nodeValue(p: RuleParam) = {
        val name = "$" + p.varName

        if (terminals contains p.ruleName) {
            val v = "(" + p.scalaClass + ")setTokenPos(new " + p.scalaClass +
                "(" + name + ".getText()" + ")," + name + ")"

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
        case (s: String) :: t if s.startsWith("{") =>
            val r = matchModifier(matchTerminalPattern, t)
            g += s
            r
        case _ =>
            matchModifier(matchTerminalPattern, tree)
    }

    /** Check whether current element comes with modifier, such as
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
    val unnamedPattern = matchNonTermPattern(null)_

    /** Takes as input list of terms. If the list starts with
      * (= ...) element, then assumes that this will become the name of
      * the next term. Otherwise, the name will be null. */
    def matchName(tree: List[Any]): List[Any]  = tree match {
        case List("=", name: String) :: t => 
            matchModifier(matchNonTermPattern(name), t)
        case t => 
            matchModifier(unnamedPattern, t)
    }

    def matchNonTermPattern(name: String)(tree: List[Any]): List[Any] =
        tree match {
            // Just rule call in the form name=Rule
            case (id: String) :: t =>
                matchRuleCall(name, id)
                t
            // More complex pattern that can contain alternatives.
            case ("(" :: alt) :: t =>
                // Sanity check
                if (name ne null)
                    error(name, "Complex pattern cannot be named")

                // Match the pattern inside parentheses.
                // The surrounding code ensures that this new pattern
                // is assumed to be in a different branch.
                g += "("
                val oldBranch = currentBranch
                currentBranch = currentBranch.extend
                matchAltList(matchName, alt)
                currentBranch = oldBranch
                g += ")"
                t
            case Nil =>
                Nil
        }

    /** Turns string 'foo' into Foo. */
    def makeKwIdentifier(s: String) = {
        val buf = new StringBuilder()
        for (i <- 1 to s.length - 1) {
            val ch = s.charAt(i)
            if (Character.isJavaIdentifierPart(ch))
                buf.append(if (i > 1) ch
                        else Character.toUpperCase(ch))
        }
        buf.toString
    }

    /** Checks whether given pattern represents literal string.
      * If yes, then adds the literal to list of known terminals and
      * keywords. Returns variable referring to that pattern.
      * If the pattern is rule call, does nothing 
     */
    def getPatternVar(pattern: String, createLiteralClass: Boolean): String = {
        println("getPatternVar(" + pattern + ")")

        // If not literal, do nothing.
        if (!(pattern startsWith "'"))
            return pattern

        // If known keyword, return the ID.
        if (keywords contains pattern)
            return keywords(pattern)

        // This literal is not in symbol table. Create identifier for it
        // and add to tables.
        var id = makeKwIdentifier(pattern)

        // Make it a valid Java identifier
        if (id != "" && !(Character isJavaIdentifierStart (id charAt 0)))
            id = "X_" + id

        // If it conflicts with existing rules, just create new variable.
        if (id == "" || (rules contains id) || (terminals contains id)) {
            println("getPatternVar: newId")
            id = newId
        }

        // Add to tables.
        keywords(pattern) = id
        terminals += id
        if (createLiteralClass) {
            rules(id) = RuleClass.terminalRule(id).withoutCodegen
        }

        id
    }

    def matchRuleCall(name: String, pattern: String) {
        println("matchRuleCall(" + name + ", " + pattern + ")")

        // Check if it is literal. If not, then patternVar = pattern.
        // If the name is not null and it is keyword, we should create
        // a case class for it. If it is just unnamed literal, then the class
        // is not necessary.
        val patternVar = getPatternVar(pattern, name ne null)
        val isLiteralPattern = pattern.startsWith("'")

        // If this is unnamed literal string...
        if (name == null && isLiteralPattern) {
            g += "\n    "
            val varName = newId
            g += "("
            g += varName + "=" + patternVar
            g += " {"
            endHook(varName, null)
            g += "})"

            return
        }

        // It was not unnamed literal.

        // Check for invalid references.
        if (!rules.contains(patternVar)) {
            error(pattern, "Reference to unknown rule: " + pattern)
            return
        }

        // Name of the variable that will be used to refer to this element.
        // If not explicitly given, then use name of the called rule.
        val varName = if (name == null) uncapitalize(patternVar) else name

        // Check whether this can be used as identifier name.
        NamingService.validateASTAttribute(varName) match {
            case Some(errorMsg) => error(pattern, errorMsg)
            case _ =>
        }

        // listVar is used for collecting values of the items, if the item
        // is a list.
        val listVar = if (isList) newId else null
        val np = RuleParam(varName, patternVar, "", listVar, currentBranch)

        // For patterns that are string literals, we do not generate separate
        // class but instead use existing LiteralNode class.
        if (isLiteralPattern) {
            np.scalaClass = "LiteralNode"
        }

        // Check if name of the node conflicts with some other name in
        // the same branch. For example:
        // x=foo y=bar x=baz is a conflict.
        // y=bar (x=foo | x=baz) is not because both x's are in different
        // branches.
        params find (_.name == varName) match {
            // This rule already has parameter with this name.
            // Check if there are conflicts.
            case Some(other) =>
                if (currentBranch.conflictsWith(other.branch)) {
                    error(pattern, "multiple tokens named '" + varName + "'")
                } else if (other.ruleName != patternVar) {
                    error(pattern, "token type conflict: " + varName +
                          " was " + other.ruleName + ", but redefined as " +
                          patternVar)
                } else {
                    println("Using existing variable name: " + other.varName)
                    np.varName = other.varName
                }
            // Parameter is unique (so far). Generate variable name for it
            // and add to the list.
            case _ =>
                println("varName")
                np.varName = newId
                params += np
        }

        g += "\n    "
        if (np.isList) {
            g += "(" + np.varName + "=" + rules(patternVar).antlrName + "{"

            var iv = ""
            if (terminals contains patternVar) {
                iv = newId
                g += "CommonNode " + iv + "=" + nodeValue(np) + ";"
            } else {
                iv = "$" + np.varName + ".r"
            }
            g += np.listVar + ".add(" + iv + ");if(_start==null)_start=" + iv

            g += ";"
            endHook(np.varName, patternVar)
            g += "})"
        } else {
            g += "("
            g += np.varName + "=" + rules(patternVar).antlrName
            g += " {"
            endHook(np.varName, patternVar)
            g+= "})"
        }
    }

    def endHook(varName: String, patternVar: String) = {
        println("endHook(" + varName + ", " + ", " + patternVar + ")")

        if (patternVar == null || (terminals contains patternVar)) {
            g += "if($" + varName + "!=null){" +
                "TokenLocation _tl = new TokenLocation(" + varName + "); " +
                "if (_start == null) _start = _tl; " +
                "if(_start.startIndex()<=_tl.endIndex()){_end=_tl.endIndex();" +
                "_endLine=_tl.endLine();_endColumn=_tl.endColumn();}}"
        } else {
            g += "if (_start == null) _start = $" + varName + ".r; " +
                "if($" + varName + ".r!=null && _start.startIndex()<=$" + varName + ".r.endIndex())" +
                "{_end=$" + varName + ".r.endIndex();" +
                "_endLine=$" + varName + ".r.endLine();_endColumn=$" + varName + ".r.endColumn();}"
        }
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
