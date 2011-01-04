package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer

import GrammarUtils._
import Actions._

abstract class NonTerminalRule(pName: String, pTree: List[Any], symbols: STable)
        extends Rule(pName, pTree, symbols) {
    override def antlrName = uncapitalize(name) + "_"
    override def ruleReturns =  " returns [" + actualReturnType  + " r]"

    def paramValue(param: RParam) = "$" + param.antlrName + ".r"
    def isTerminalRule = false

    protected def wrapInReturn(expr: String) =
        if (returnCode ne null)
            symbols.getGrammarName + "Grammar.return" + name +"(" + expr + ")"
        else
            expr
}

class OptionRule(pName: String, pTree: List[Any], symbols: STable)
        extends NonTerminalRule(pName, pTree, symbols) {
    import symbols._

    /** Finds and records all the rule parameters. */
    def collectParams() {
        // Nothing to do: tree already contains list of called rules.
    }

    override def generateClasses() {
        classes(name) = new RClass(name, "trait", body)
        for (opt <- tree) {
            actions.addBinding(opt.toString, addExtend(name))

            if (!rules.contains(opt.toString)) {
                error(opt, "Reference to undefined rule " + opt)
            }

            // If called rule has some weird return type then we
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

            val param = new RParam("", option, null, false, symbols)

            buf += param.antlrName + "=" + rules(option).antlrName +
                    "{$r=" + wrapInReturn(rules(param.rule).paramValue(param)) +
                    ";}"
            first = false
        }
    }
}

object Modifier extends Enumeration("?", "*", "+") {
    type Val = Value

    val Optional, Star, Plus = Value
}

class NormalRule(pName: String, pTree: List[Any], symbols: STable)
        extends NonTerminalRule(pName, pTree, symbols) {
    import symbols._

    /** Identifies current branch in the options. */
    var currentBranch: BranchIdentifier = BranchIdentifier.empty

    /** whether the currently analyzed branch contains
      * repetition (Foo+ or Foo*). */
    var isList = false

    /** Finds and records all the rule parameters. */
    def collectParams() {
        collectOptionList(tree)
    }

    def collectOptionList(lst: List[Any]) {
        for ("NODE" :: matches <- lst) {
            for (m <- matches) {
                m match {
                    case List("MATCH", modifier: String, ruleCall) =>
                        doMatch(Modifier.withName(modifier), ruleCall)
                    case List("MATCH", ruleCall) =>
                        doMatch(null, ruleCall)
                }
            }
            currentBranch = currentBranch.nextBranch
        }
    }

    def doMatch(modifier: Modifier.Val, ruleCall: Any) {
        val oldList = isList
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

                currentBranch = currentBranch.extend
                collectOptionList(options)

                currentBranch = oldBranch
        }
        isList = oldList
    }

    def doRuleCall(name: String, pattern: String) {
        println("doRuleCall(" + name + ", " + pattern + "), branch=" +
                currentBranch + ", list=" + isList)

        // Unnamed call to literal "foo"
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

        val paramName = getParamName(name, pattern)

        val param = new RParam(paramName, calledRuleName, currentBranch, isList,
                symbols)

        checkParamNameConflicts(param)
    }

    def addLiteralRule(pattern: String) {
        if (keywords.contains(pattern)) {
            return
        }

        val ruleName = makeKeywordName(pattern)
        rules(ruleName) = new LiteralRule(ruleName, pattern, symbols)
        keywords(pattern) = ruleName
    }

    /** Turns string 'foo' into Foo. */
    def makeKeywordName(s: String) = {
        val buf = new StringBuilder()
        for (i <- 1 to s.length - 1) {
            val ch = s.charAt(i)
            if (Character.isJavaIdentifierPart(ch))
                buf.append(if (!buf.isEmpty) ch
                        else Character.toUpperCase(ch))
        }
        var id = buf.toString

        // Make it a valid Java identifier
        if (id != "" && !(Character isJavaIdentifierStart (id charAt 0)))
            id = "X_" + id

        // If it conflicts with existing rules, just create new variable.
        if (id == "" || (rules.contains(id))) {
            id = newId
        }

        id
    }

    // Check if name of the node conflicts with some other name in
    // the same branch. For example:
    // x=foo y=bar x=baz is a conflict.
    // y=bar (x=foo | x=baz) is not because both x's are in different
    // branches.
    def checkParamNameConflicts(np: RParam) {
        println("checkParamNameConflicts(" + np.name + ", " + np.rule + ")")
        val varName = np.name

        params find (_.name == varName) match {
            // This rule already has parameter with this name.
            // Check if there are conflicts.
            case Some(other) =>
                if (currentBranch.conflictsWith(other.branch)) {
                    throw new Exception("multiple tokens named '" + varName + "'")
                } else if (other.rule != np.rule) {
                    // TODO: better check for type conflict.
                    throw new Exception("token type conflict: " + varName +
                          " was " + other.rule + ", but redefined as " +
                          np.rule)
                } else {
                    // The compatible parameter already exists, do nothing.
                    ()
                }
            case _ =>
                params += np
        }
    }

    override def generateClasses() {
        println("generateClasses(" + name + ")")
        val cl = new RClass(name, "case class", body)
        classes(name) = cl

        for (p <- params) {
            println("param: " + p)
            if (!rules.contains(p.rule)) {
                throw new Exception("Invalid rule reference: " + name + "." +
                        p.name)
            }
            val ruleType = rules(p.rule).actualReturnType
            cl.params += new RCParam(p.name,
                    if (p.isList) "List[" + ruleType + "]" else ruleType)
        }

        super.generateClasses()
    }

    override def ruleInit(implicit buf: ArrayBuffer[String]) {
        buf += "\n@init {SourceLocation _start=null;"
        buf += "int _end=-1;int _endLine=-1;int _endColumn=-1;"
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
        buf += "$r.setStart(_start);"
        buf += "$r.setEnd(_end==-1?(_start==null?0:_start.endIndex()):_end,"
        buf += "_endLine==-1?(_start==null?0:_start.endLine()):_endLine,"
        buf += "_endColumn==-1?(_start==null?0:_start.endColumn()):_endColumn);}"
    }

    override def ruleBody(implicit buf: ArrayBuffer[String]) {
        doOptionList(generateRuleCall, tree)
    }

    def generateRuleCall(node: Any)(implicit buf: ArrayBuffer[String]) {
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

//            val patternVar = newId
//
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

    private def isLiteralPattern(pattern: String) = pattern.startsWith("'")

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
