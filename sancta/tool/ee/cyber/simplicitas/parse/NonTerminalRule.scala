// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

import GrammarUtils._
import Actions._

/** Base class for normal and option rules. */
abstract class NonTerminalRule(pName: String, pTree: List[Any],
                               symbols: SymbolTable)
        extends Rule(pName, pTree, symbols) {
    override def antlrName = uncapitalize(name) + "_"
    override def ruleReturns =  " returns [" + actualReturnType  + " r]"

    def paramValue(param: RuleParam) = "$" + param.antlrName + ".r"
    def isTerminalRule = false

    /** Wrap code in expr to call to "returns expression" of this rule. */
    protected def wrapInReturn(expr: String) =
        if (returnCode ne null)
            symbols.getGrammarName + "Grammar.return" + name +"(" + expr + ")"
        else
            expr
}

/** Option rules:
 *
 * option Foo: Bar | Baz;
 */
class OptionRule(pName: String, pTree: List[Any], symbols: SymbolTable)
        extends NonTerminalRule(pName, pTree, symbols) {
    import symbols._

    def collectParams() {
        // Nothing to do: tree already contains list of called rules.
    }

    override def generateClasses() {
        // Create trait for this rule.
        classes(name) = new RuleClass(name, "trait", body)

        for (opt <- tree) {
            // Make the called rule extend the trait.
            actions.addBinding(opt.toString, addExtend(name))

            if (!rules.contains(opt.toString)) {
                error(opt, "Reference to undefined rule " + opt)
            }

            // If the called rule has some weird return type then we
            // must extend this return type, otherwise the rule call
            // will not be type correct.
            val calledRule = rules(opt.toString)
            if (calledRule.returnType ne null) {
                actions.addBinding(actualReturnType,
                        addExtend(calledRule.returnType))
            }
        }

        super.generateClasses()
    }

    override def ruleBody(implicit buf: ArrayBuffer[String]) {
        var first = true

        for (opt <- tree) {
            // Assuming here that option rule contains just list of options.
            val option = opt.toString

            if (!first) {
                buf += "\n    | "
            }

            val param = new RuleParam("", option, null, false, symbols)

            buf += param.antlrName + "=" + rules(option).antlrName +
                    "{$r=" + wrapInReturn(rules(param.rule).paramValue(param)) +
                    ";}"
            first = false
        }
    }
}

/** Possible multiplicity modifiers for patterns. */
object Modifier extends Enumeration("?", "*", "+") {
    type Val = Value

    val Optional, Star, Plus = Value
}

/**Normal nonterminal rule in the form:
 *
 * Foo: bar=Bar baz=Baz+
 */
class NormalRule(pName: String, pTree: List[Any], symbols: SymbolTable)
        extends NonTerminalRule(pName, pTree, symbols) {
    import symbols._

    /** Identifies current branch in the options. It is only used during
      * the collectParams() method. */
    var currentBranch: BranchIdentifier = BranchIdentifier.empty

    /** whether the currently analyzed branch contains
     * repetition (Foo+ or Foo*). */
    var isList = false

    def collectParams() {
        collectOptionList(tree)
    }

    private def collectOptionList(lst: List[Any]) {
        // Iterate through all the options...
        for ("NODE" :: matches <- lst) {
            for (m <- matches) {
                m match {
                    // Pattern with ?, + or * modifier.
                    case List("MATCH", modifier: String, ruleCall) =>
                        doMatch(Modifier.withName(modifier), ruleCall)
                    // Just pattern.
                    case List("MATCH", ruleCall) =>
                        doMatch(null, ruleCall)
                }
            }
            currentBranch = currentBranch.nextBranch
        }
    }

    /** Processes the pattern in the ruleCall parameter. */
    private def doMatch(modifier: Modifier.Val, ruleCall: Any) {
        val oldList = isList
        // Check whether the current pattern has more than one occurrence.
        if ((modifier eq Modifier.Plus) || (modifier eq Modifier.Star)) {
            isList = true
        }

        ruleCall match {
            // Foo
            case pattern: String =>
                doRuleCall(null, pattern)
            // foo=Bar
            case List("=", name: String, pattern: String) =>
                doRuleCall(name, pattern)
            // (foo)
            case "(" :: options =>
                val oldBranch = currentBranch

                // The pattern in parentheses will be in a new branch.
                currentBranch = currentBranch.extend
                // Recursively process the pattern.
                collectOptionList(options)

                currentBranch = oldBranch
        }
        isList = oldList
    }

    /** By now we have isolated pattern in the form "name=pattern"
     * where name may be null (in case of unnamed pattern) and the pattern
     * is definitely a call to other rule. */
    private def doRuleCall(name: String, pattern: String) {
        // Unnamed call to literal: just "foo"
        if ((name eq null) && isLiteralPattern(pattern)) {
            addLiteralRule(pattern)
            return
        }

        var calledRuleName = pattern

        // handle calls to named literals: foo="bar"
        if (pattern.startsWith("'")) {
            addLiteralRule(pattern)
            calledRuleName = keywords(pattern)
        }

        /** What's the name of the parameter? */
        val paramName = getParamName(name, pattern)

        val param = new RuleParam(paramName, calledRuleName,
                currentBranch, isList, symbols)

        // Check whether this rule call conflicts with other rule calls
        // with the same name.
        checkParamNameConflicts(pattern, param)
    }

    /** Processes call to unnamed literal. This literal will not go to AST.
     * We just have to create rule for it.
     */
    private def addLiteralRule(pattern: String) {
        // It's already registered.
        if (keywords.contains(pattern)) {
            return
        }

        // Make legal name for it.
        val ruleName = makeKeywordName(pattern)

        // Create new rule.
        rules(ruleName) = new LiteralRule(ruleName, pattern, symbols)
        keywords(pattern) = ruleName
    }

    /** Creates identifier for the keyword. If keyword consists of characters,
     * uses the keyword itself. If keyword contains punctuation or other
     * similar stuff, generate new unique ID. Unique ID is also generated if
     * rule with this name already exists. */
    private def makeKeywordName(s: String): String = {
        // Let's see if we can make it Java identifier...
        var id = s.filter(isIdentifierPart).capitalize

        // Make it a valid Java identifier
        if (id != "" && !(isIdentifierStart(id(0))))
            id = "X_" + id

        if (id == "" || (rules.contains(id)))
            // It is unintelligible or conflicts with existing rule.
            // In this case, create a new variable.
            newId
        else
            id
    }

    /** Check if name of the node conflicts with some other name in
     * the same branch. For example:
     * x=foo y=bar x=baz is a conflict.
     * y=bar (x=foo | x=baz) is not because both x's are in different
     * branches. */
    private def checkParamNameConflicts(loc: Object, np: RuleParam) {
        val varName = np.name

        params find (_.name == varName) match {
            // This rule already has parameter with this name.
            // Check if there are conflicts.
            case Some(other) =>
                if (currentBranch.conflictsWith(other.branch)) {
                    // There are two rule calls with the same name in the
                    // same branch.
                    error(loc,
                        "Rule \"" + name +
                            "\" contains several calls with the same parameter name: " +
                            varName)
                } else if (other.rule != np.rule) {
                    // There is rule call with same name and it has different
                    // type. Hence, we cannot generate meaningful
                    // AST classes and grammar.
                    error(loc, "In rule \"" + name + "\", parameter " +
                            varName + " is used to call different rules: " +
                            np.rule + " and " + other.rule)
                } else {
                    // Rule call with same name exists, but it has compatible
                    // type. We are happy and need to do nothing more.
                    ()
                }
            // No similar parameter found, add it to the list.
            case _ =>
                params += np
        }
    }

    override def generateClasses() {
        // There will always be a case class generated for this rule, even
        // if the return type of the rule is something else.
        val cl = new RuleClass(name, "case class", body)
        classes(name) = cl

        for (p <- params) {
            if (!rules.contains(p.rule)) {
                error(name, "Invalid rule reference: " + name + "." +
                        p.name)
            }

            // The generated class will have parameters whose type is
            // the declared return type of the rule (not just rule name).
            val ruleType = rules(p.rule).actualReturnType
            cl.params += new RuleClassParam(p.name,
                    if (p.isList) "List[" + ruleType + "]" else ruleType)
        }

        super.generateClasses()
    }

    override def ruleInit(implicit buf: ArrayBuffer[String]) {
        buf += "\n@init {SourceLocation _start=null;"
        buf += "int _end=-1;int _endLine=-1;int _endColumn=-1;"

        // For each list parameter, generate a new variable whose
        // value will accumulate the list contents.
        for (p <- params if p.isList) {
            buf += "ArrayList " + p.listVar + "=new ArrayList();"
        }
        buf += "}"
    }

    override def ruleAfter(implicit buf: ArrayBuffer[String]) {
        buf += "\n@after {$r = "
        buf += wrapInReturn("new " + name + "(" + join(params.map(
                p => if (p.isList)
                    "scalaList(" + p.listVar + ")"
                    else
                        rules(p.rule).paramValue(p)
                    )) + ")") + ";"
        buf += "if ($r != null) {"
        buf += "$r.setStart(_start);"
        buf += "$r.setEnd(_end==-1?(_start==null?0:_start.endIndex()):_end,"
        buf += "_endLine==-1?(_start==null?0:_start.endLine()):_endLine,"
        buf += "_endColumn==-1?(_start==null?0:_start.endColumn()):_endColumn);}}"
    }

    def ruleBody(implicit buf: ArrayBuffer[String]) {
        doOptionList(generateRuleCall, tree)
    }

    private def generateRuleCall(node: Any)(implicit buf: ArrayBuffer[String]) {
        def generate(name: String, pattern: String) {
            val ruleName: String =
                if (isLiteralPattern(pattern))
                    keywords(pattern)
                else
                    pattern

            buf += "\n    "

            // Unnamed call to literal "foo"
            if ((name eq null) && isLiteralPattern(pattern)) {
                val varName = newId

                buf += "("
                buf += varName + "=" + ruleName
                buf += " {"
                endHook(varName, null)
                buf += "})"

                return
            }

            val paramName = getParamName(name, pattern)
            val param = getParamByName(paramName)

            if (param.isList) {
                buf += "(" + param.antlrName + "=" + rules(param.rule).antlrName + "{"

                var iv = ""
                if (rules(param.rule).isTerminalRule) {
                    iv = newId
                    buf += "CommonNode " + iv + "=" + rules(param.rule).paramValue(param) + ";"
                } else {
                    iv = "$" + param.antlrName + ".r"
                }
                buf += param.listVar + ".add(" + iv + ");if(_start==null)_start=" + iv

                buf += ";"
                endHook(param.antlrName, param.rule)
                buf += "})"
            } else {
                buf += "("
                buf += param.antlrName + "=" + rules(param.rule).antlrName
                buf += " {"
                endHook(param.antlrName, param.rule)
                buf += "})"
            }
        }

        def endHook(varName: String, patternVar: String)
                (implicit buf: ArrayBuffer[String]) = {
            if (patternVar == null || rules(patternVar).isTerminalRule) {
                buf += "if($" + varName + "!=null){" +
                    "TokenLocation _tl = new TokenLocation(" + varName + "); " +
                    "if (_start == null) _start = _tl; " +
                    "if(_start.startIndex()<=_tl.endIndex()){_end=_tl.endIndex();" +
                    "_endLine=_tl.endLine();_endColumn=_tl.endColumn();}}"
            } else {
                buf += "if (_start == null) _start = $" + varName + ".r; " +
                    "if($" + varName + ".r!=null && _start.startIndex()<=$" + varName + ".r.endIndex())" +
                    "{_end=$" + varName + ".r.endIndex();" +
                    "_endLine=$" + varName + ".r.endLine();_endColumn=$" + varName + ".r.endColumn();}"
            }
        }

        // The actual body of the generateRuleCall method
        node match {
            // Foo
            case pattern: String =>
                generate(null, pattern)
            // foo=Bar
            case List("=", name: String, pattern: String) =>
                generate(name, pattern)
            // (foo)
            case "(" :: options =>
                buf += "("
                doOptionList(generateRuleCall, options)
                buf += ")"
        }
    }

    /** Return true, if the pattern corresponds to just string literal. */
    private def isLiteralPattern(pattern: String) = pattern.startsWith("'")

    /** Get the name of parameter in the rule call. */
    private def getParamName(name: String, pattern: String) =
        if (name eq null)
            uncapitalize(pattern)
        else
            name

    private def getParamByName(name: String) =
        params.find(_.name == name) match {
            case Some(ret) => ret
            case None => throw new IllegalArgumentException(
                    "Cannot find rule parameter " + name)
        }
}
