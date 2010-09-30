// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.parse

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

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

case class ConstructorParam(name: String, vtype: String, code: String,
        mod: String)

class RuleClass(name: String) {
    var hdr = ""
    var param: Seq[ConstructorParam] = Nil
    val extend = new ArrayBuffer[String]()
    var body = ""
    def antlrName = name
}

class GrammarException(msg: String) extends Exception(msg)

object RepeatType extends Enumeration {
    type Type = Value
    val None, List, Optional = Value
}

class GrammarGen(posMap: Any => List[Int]) {
    private var grammarName = "";
    private var grammarPackage = "";
    private var g = new ArrayBuffer[String]()
    private val treeSrc = new StringBuilder()
    private val param = new ArrayBuffer[NodeParam]()

    /** Set of used terminal identifiers. */
    private val terminals = collection.mutable.Set.empty[String]

    /** Rules in this language, indexed by rule name. */
    private val rules = new HashMap[String, RuleClass]()

    /** Keywords supported by this language. Map is from keyword
      * to identifier representing the corresponding lexer rule. */
    private val keywords = new HashMap[String, String]()

    private var currentOption = List(0)
    private var multi = RepeatType.None
    private var firstInChain = true
    private var idcounter = 0

    /** First rule in the grammar. This will become the start symbol. */
    private var firstRule = ""

    /** Grammar-level options that are passed to ANTLR. */
    private var grammarOptions = ""

    def newId = {
        idcounter += 1
        println("newId(" + "Z" + idcounter + ")")
        "Z" + idcounter
    }

    def uncapitalize(s: String): String =
        if (s == "")
            ""
        else
            (Character toLowerCase (s charAt 0)) + (s substring 1)

    def error(where: Any, what: String) =
        throw new GrammarException(posMap(where) match {
                case List(line, col) =>
                    line + ":" + col + ": " + what
                case _ =>
                    what
            })

    def endHook(tag: String, embrace: Boolean, id: String) = {
        println("endHook(" + tag + ", " + id + ")")
        val p = g.size - 1
        var t = tag;
        if (t == "") {
            t = newId
            g(p) = t + "=" + g(p)
        }
        val code = 
            if (id == null || (terminals contains id))
                "if($" + t + "!=null){if (_start == null) _start = new TokenLocation(" + t + ");" +
                "TokenLocation _tl = new TokenLocation(" + t + ");" +
                "if(_start.startIndex()<=_tl.endIndex()){_end=_tl.endIndex();" +
                "_endLine=_tl.endLine();_endColumn=_tl.endColumn();}}"
            else
                "if($" + t + ".r!=null && _start.startIndex()<=$" + t + ".r.endIndex())" +
                "{_end=$" + t + ".r.endIndex();" +
                "_endLine=$" + t + ".r.endLine();_endColumn=$" + t + ".r.endColumn();}"
        if (embrace) {
            g(p) = "(" + g(p) + "{" + code + "})"
        } else {
            g(p) = g(p) + code
        }
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

    def trKeyword(keyword: String): String = {
        println("trKeyword(" + keyword + ")")
        if (!(keyword startsWith "'"))
            return keyword
        if (keywords contains keyword)
            return keywords(keyword)
        var id = makeKwIdentifier(keyword)
        println("namebuf=\"" + id + "\"")
        if (id != "" &&
                !(Character isJavaIdentifierStart (id charAt 0)))
            id = "X_" + id
        if (id == "" || (rules contains id) || (terminals contains id)) {
            println("trKeyword: newId")
            id = newId
        }
        keywords(keyword) = id
        terminals += id
        id
    }

    def simpleTerm(name: String, _id: String) {
        println("simpleTerm(" + name + ", " + _id + ")")
        val id = trKeyword(_id)        
        if (name == null && (_id startsWith "'")) {
            g += " "
            val term =
            if (firstInChain) {
                println("firstInChain")
                val tag = newId
                g += "(" + tag + "=" + id + "{"
                if (multi != RepeatType.None)
                    g += "if(_start==null)"
                g += "_start=new TokenLocation($" + tag + ");"
                endHook(tag, false, null)
                g += "})"
            } else {
                g += id
                endHook("", true, null)
            }
            return
        }
        val tagName = if (name == null) uncapitalize(id); else name
        println("tmpName, multi = " + multi + ", firstInChain = " + firstInChain)
        val tmpName =
            if (multi == RepeatType.List || firstInChain) newId
            else null

        NamingService.validateASTAttribute(tagName) match {
            case Some(errorMsg) => error(_id, errorMsg)
            case _ =>
        }
        val np = NodeParam(tagName, id, "", multi == RepeatType.List,
                tmpName, currentOption)
        param find (_.name == tagName) match {
            case Some(other) =>
                if ((other.option zip currentOption) exists
                        ((a: Tuple2[Int, Int]) => a._1 == a._2))
                    error(_id, "multiple tokens named '" + tagName + "'")
                else if (other.node != id)
                    error(_id, "token type conflict: " + tagName +
                          " was " + other.node + ", but redefined as " + id)
                else
                    np.varName = other.varName
            case _ =>
                println("varName")
                np.varName = newId
                param += np
        }
        if (tmpName ne null) {
            g += "\n(" + np.varName + "=" + rules(id).antlrName + "{"
            // must be firstInChain
            if (multi == RepeatType.None || multi == RepeatType.Optional) {
                if (multi == RepeatType.None) {
                    g += "_start=" + tmpName + "=" + nodeValue(np)
                } else if (multi == RepeatType.Optional ) {
                    g += tmpName + "=" + nodeValue(np) +
                        ";if(_start==null)_start=" + tmpName
                }
            } else if (!firstInChain) {  // only multi
                g += tmpName + ".add(" + nodeValue(np) + ")"
            } else { // both multi and firstinchain
                var iv = ""
                if (terminals contains id) {
                    println("iv")
                    iv = newId
                    g += "CommonNode " + iv + "=" + nodeValue(np) + ";"
                } else {
                    iv = "$" + np.varName + ".r"
                }
                g += tmpName + ".add(" + iv + ");if(_start==null)_start=" + iv
            }
            g += ";"
            endHook(np.varName, false, id)
            g += "})"
        } else {
            g += " "
            g += np.varName + "=" + rules(id).antlrName
            endHook(np.varName, true, id)
        }
    }

    def simple(name: String)(tree: List[Any]): List[Any] = tree match {
        case (id: String) :: t =>
            simpleTerm(name, id)
            firstInChain = false
            t
        case ("(" :: alt) :: t =>
            if (name ne null)
                error(name, "The following pattern cannot be given a name")
            g += "("
            currentOption = currentOption ++ List(0)
            altList(matchName, alt)
            currentOption = currentOption dropRight 1
            g += ")"
            firstInChain = false
            t
        case Nil =>
            Nil
    }

    def matchModifier(f: List[Any] => List[Any], tree: List[Any]): List[Any] = {
        def after(v: List[Any], block: String): List[Any] = {
            val oldMulti = multi
            val oldFirst = firstInChain
            if (block != "?")
                multi = RepeatType.List
            val result = f(v)
            if (block != "+") {
                firstInChain = oldFirst
                multi = 
                    if (oldMulti == RepeatType.List)
                        oldMulti
                    else
                        RepeatType.Optional
            } else {
                multi = oldMulti
            }
            g += block
            result
        }

        tree match {
            case "?" :: t => after(t, "?")
            case "*" :: t => after(t, "*")
            case "+" :: t => after(t, "+")
            case t => f(t)
        }
    }

    val simple_null = simple(null)_

    def matchName(tree: List[Any]): List[Any]  = tree match {
        case List("=", name: String) :: t => 
            matchModifier(simple(name), t)
        case t => 
            matchModifier(simple_null, t)
    }

    def altList(doMatch: List[Any] => List[Any], tree: List[Any]) {
        var first = true
        var startingFirst = firstInChain
        for ("NODE" :: matches <- tree) {
            firstInChain = startingFirst
            if (!first) {
                g += "|"
                currentOption = (currentOption dropRight 1) ++
                    List(currentOption.last + 1)
            }
            var i = matches
            while (!i.isEmpty) {
                i = doMatch(i)
            }
            first = false
        }
    }

    def join(i: Iterable[String]) =
        if (i isEmpty)
            ""
        else
            i reduceLeft (_ + ", " + _)

    // stupid lowlevel replace...
    def replaceAll(src: String, needle: String, replacement: String): String = {
        val buf = new StringBuilder()
        var end = 0
 
        while (true) {
            val pos = src.indexOf(needle, end)
            if (pos < 0) {
                buf append src.substring(end)
                return buf toString
            }
            buf append src.substring(end, pos)
            buf append replacement
            end = pos + needle.length
        }
        ""
    }

    def nodeValue(p: NodeParam) = {
        val name = "$" + p.varName

        if (terminals contains p.node) {
            val v = "(" + p.node + ")setTokenPos(new " + p.node + "(" +
                name + ".getText()" + ")," + name + ")"

            if (p.tmp ne null)
                v
            else 
                name + "==null?null:" + v
        } else {
            name + ".r"
        }
    }

    def caseParam(p: NodeParam) = {
        ConstructorParam(p.name, if (p.isList) "List[" + p.node + "]" else p.node,
                  "", "var ")
    }

    /** Checks whether this rule is the first rule that is destined
      * to be grammar's start symbol. */
    def checkFirstRule(name: String) {
        if (firstRule == "")
            firstRule = name
    }

    def matchNormalRule(name: String, alt: List[Any]) {
        println("normal rule: " + name + ": " + alt)

        checkFirstRule(name)

        currentOption = List(0)
        firstInChain = true
        multi = RepeatType.None
        g += "\n" + rules(name).antlrName + " returns [" + name +
            " r]\n@init {SourceLocation _start=null;int _end=-1;" +
            "int _endLine=-1;int _endColumn=-1;"
        val init_p = g.size
        g += ""
        g += "}\n@after {$r = new " + name + "("
        val p = g.size
        g += ""
        g += ");$r.setLocation(_start,_end==-1?(_start==null?0:_start.endIndex()):_end," +
            "_endLine==-1?(_start==null?0:_start.endLine()):_endLine," +
            "_endColumn==-1?(_start==null?0:_start.endColumn()):_endColumn);}:\n"
        altList(matchName, matchCodeBlock(alt, name))
        g += ";\n"
        val init = new StringBuilder()
        def getParam(p: NodeParam): String = {
            println("getParam(" + p + ")")
            if (p.tmp == null)
                return nodeValue(p)
            if (p.isList) {
                init append ("ArrayList " + p.tmp + "=new ArrayList();")
                "scalaList(" + p.tmp + ")"
            } else {
                init append (p.node + " " + p.tmp + "=null;")
                p.tmp
            }
        }
        g(p) = join(param map getParam)
        g(init_p) = init.toString
        rules(name).hdr = "case class " + name
        rules(name).param = param map caseParam
        for (p <- param) {
            if (!(rules contains p.node)) {
                error(p.node, "Undefined rule " + p.node + " referenced")
            }
        }
        param clear
    }

    def matchOptionRule(name: String, alt: List[Any]) {
        println("option: " + name + ": " + alt)

        checkFirstRule(name)

        g += "\n" + rules(name).antlrName + " returns [" + name +
            " r]:\n"

        val optionList = matchCodeBlock(alt, name)
        var first = true

        for (t <- optionList) {
            val option = t.toString

            if (!first)
                g += " | "

            if (!(rules contains option))
                error(t, "Undefined rule " + option + " referenced")

            val r = rules(option)
            if (!(r.extend contains name))
                r.extend += name
            println("t(" + option + ")")
            val id = newId
            val np = NodeParam(id, option, id, false, null, Nil)
            g += id + "=" + r.antlrName + "{$r=" + nodeValue(np) + ";}"
            first = false
        }
        g += ";\n"
    }

    def rule(tree: Any) = tree match {
        case ":" :: (name: String) :: alt =>
            matchNormalRule(name, alt)
        case "option" :: (name: String) :: alt =>
            matchOptionRule(name, alt)
        case _ =>
            ()
    }

    def terminal(tree: List[Any]): List[Any] = tree match {
        case h :: t => h match {
            case s: String => s match {
                case "~" =>
                    g += " ~"
                    terminal(t)
                case _ =>
                    g += " "
                    g += s  // identifier
                    t
            }
            case l: List[Any] => l match {
                case "(" :: alt =>
                    g += "("
                    altList(termAction, alt)
                    g += ")"
                    t
                case ".." :: (from: String) :: (to: String) :: Nil =>
                    g += " " + from + ".." + to
                    t
                case _ => // Just to satisfy the compiler.
                    throw new IllegalArgumentException()
            }
        }
        case Nil => Nil
    }

    def termAction(tree: List[Any]): List[Any] = tree match {
        case (s: String) :: t if s startsWith "{" =>
            val r = matchModifier(terminal, t)
            g += s
            r
        case _ =>
            matchModifier(terminal, tree)
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

    def termDef(isFragment: Boolean, ruleName: String, alt: List[Any], 
            addHidden: Boolean) {
        val termClass = new RuleClass(ruleName)

        if (isFragment) {
            g += "fragment "
        } else {
            termClass.extend += "TerminalNode"
            rules(ruleName) = termClass
        }
        g += ruleName + ':'
        altList(termAction, matchCodeBlock(alt, ruleName))
        g += (if (addHidden) "{$channel = HIDDEN;}" else "") + ";\n"
        if (!isFragment) {
            val l = List(ConstructorParam("text", "String", "$_", ""))
            termClass.hdr = "case class " + ruleName
            termClass.param = l
            terminals += ruleName
        }
    }

    /** Checks whether given name can safely be used as identifier, 
      * i.e., it does not clash with Scala keyword. */
    private def checkName(isTerminal: Boolean, typeName: String, name: String, 
            tree: Any) =
        NamingService.validateRuleName(name, typeName, isTerminal) match {
            case Some(errorMsg) => 
                error(tree, errorMsg)
            case _ =>
        }

    /** Parses given rule and adds it to global <code>rules</code> map.
      * XXX: for terminals, also generates the code. */
    def addToRuleTable(tree: Any) = tree match {
        case "terminal" :: "hidden" :: (name: String) :: alt =>
            checkName(true, "Terminal", name, tree)
            termDef(false, name, alt, true)
        case "terminal" :: (name: String) :: alt =>
            checkName(true, "Terminal", name, tree)
            termDef(false, name, alt, false)
        case "fragment" :: (name: String) :: alt =>
            checkName(true, "Fragment", name, tree)
            termDef(true, name, alt, false)
        case "option" :: (name: String) :: _ =>
            checkName(false, "Option", name, tree) 
            val r = new RuleClass(uncapitalize(name) + "_")
            r.hdr = "trait " + name
            rules(name) = r
        case ":" :: (name: String) :: _ =>
            checkName(false, "Rule", name, tree)
            rules(name) = new RuleClass(uncapitalize(name) + "_")
        case _ => 
            ()
    }

    def grammargen(tree: Object) {
        val terms = g
        terminals += "EOF"
        tree match {
            case ("grammar" :: nameParts) :: rest =>
                matchGrammarName(nameParts, tree)

                val ruleList = matchGrammarOptions(rest)

                ruleList foreach addToRuleTable
                /* Reset the output source code, the previously generated
                 * code will remain in variable terms. */
                g = new ArrayBuffer[String]()
                g += grammarHeader
                ruleList foreach rule
        }
        for (kw <- keywords.keys)
            g += keywords(kw) + ": " + kw + ";\n"
        println("terms = " + terms)
        g ++= terms
        grammarClass
    }

    /** Parses the full name of the grammar. Fills grammarName and
      * grammarPackage variables. */
    def matchGrammarName(nameParts: List[Any], tree: Object) {
        nameParts.reverse match {
            case (name: String) :: "." :: pname =>
                grammarName = name
                grammarPackage = (pname.reverse foldLeft "")(_+_)
            case List(name: String) => 
                grammarName = name
            case _ => 
                error(tree, "no grammar name")
        }
    }

    /** Parses grammar-level "options(foo = bar;)" declaration. */
    def matchGrammarOptions(tree: Any) = tree match {
        case ("options" :: opts) :: rules =>
            for (List(name, value) <- opts) {
                grammarOptions += " " + name + "=" + value + ";"
            }
            rules
        case rules: List[Any] => 
            rules
    }

    def getTreeSource = treeSrc.toString

    def getGrammarName = grammarName

    def getGrammarSource = {
        val buffer = new StringBuilder()
        g foreach (buffer append _)
        buffer.toString
    }

    def grammarHeader =
        "grammar " + grammarName + ";\noptions { " +
        "superClass=ParserBase; " + grammarOptions + " }" +
        "\n@header {\npackage " + grammarPackage +
        ";\n import java.util.ArrayList;\n" +
        " import ee.cyber.simplicitas.CommonNode;\n" +
        " import ee.cyber.simplicitas.parse.ParserBase;\n" +
        " import ee.cyber.simplicitas.SourceLocation;\n" +
        " import ee.cyber.simplicitas.parse.TokenLocation;\n}\n" +
        "@lexer::header { package " + grammarPackage + """; }
    @lexer::members {
        private ee.cyber.simplicitas.parse.ErrorHandler handler = null;
        public void reportError(RecognitionException ex) {
            handler.reportError(ex);
            super.reportError(ex);
        }

        public void emitErrorMessage(String s) {
            handler.emitErrorMessage(s);
        }

        public void setErrorHandler(ee.cyber.simplicitas.parse.ErrorHandler handler) {
            this.handler = handler;
        }

        public void displayRecognitionError(String[] tokenNames,
                RecognitionException e) {
            emitErrorMessage(getErrorMessage(e, tokenNames));
        }
    }
    """

    def grammarClass {
        treeSrc append ("package " + grammarPackage + ";\n\n" +
                        "import ee.cyber.simplicitas." +
                          "{CommonNode, CommonToken, TerminalNode}\n" +
                        "import ee.cyber.simplicitas.parse." +
                          "{ErrorHandler}\n\n")

        for (r <- rules.values) {
            treeSrc append r.hdr
            if (!r.param.isEmpty)
                treeSrc append ("(" + join(
                    r.param map (t => t.mod + t.name + ": " + t.vtype)) + ")")
            treeSrc append " extends "
            r.extend.toList match {
                case Nil =>
                    treeSrc append "CommonNode"
                case h :: t =>
                    treeSrc append h
                    if (!t.isEmpty)
                        treeSrc append (" with " + join(t))
            }
            treeSrc append " {"
            if (r.hdr startsWith "case ")
                treeSrc append ("\n  def childrenNames = Array(" +
                    join(r.param map ('"' + _.name + '"')) + ");\n")
            treeSrc append (r.body + "}\n")
        }
        treeSrc append ("\nobject " + grammarName +
                        "Kind extends Enumeration {\n  type Kind = Value;\n")
        for (t <- terminals)
            treeSrc append ("  val " + t + " = Value(" + grammarName +
                            "Lexer." + t + ");\n")
        treeSrc append
           ("}\n\nclass " + grammarName + "Grammar extends " +
                "ee.cyber.simplicitas.parse.GrammarBase[" +
                firstRule + ", " + grammarName + "Kind.Kind] {\n" +
            "  type Token = CommonToken[" + grammarName + "Kind.Kind]\n" +
            "  def lexer(source: org.antlr.runtime.CharStream, errorHandler:" +
                " ErrorHandler): org.antlr.runtime.TokenSource = {\n" +
                "    val lex = new " + grammarName + "Lexer(source)\n" +
                "    lex.setErrorHandler(errorHandler)\n" +
                "    lex\n  }\n" +
            "  def doParse(tokens: org.antlr.runtime.TokenStream," +
                " errorHandler: ErrorHandler): " + firstRule + " = {\n" +
                "    val parser = new " + grammarName + "Parser(tokens)\n" +
                "    parser.errorHandler = errorHandler\n" +
                "    parser." + rules(firstRule).antlrName + "()\n" +
                "  }\n" +
            "  def tokenType(token: Int): " + grammarName + "Kind.Kind =\n" +
            "    " + grammarName + "Kind(token);\n" +
            "  def tokenKind(token: Int): Int = token match {\n")
        val reallyKeywords = new ArrayBuffer[String]()
        for (kw <- keywords.keys) {
            val what =
                if (Character isJavaIdentifierPart (kw charAt 1)) {
                    reallyKeywords += kw.substring(1, kw.length - 1)
                    "keyword"
                } else {
                    "operator"
                }
            treeSrc append ("    case " + grammarName + "Lexer." +
                            keywords(kw) + " => " + what + ";\n")
        }
        treeSrc append "    case _ => normal;\n  }\n" +
            "  val keywords: Seq[String] = Array[String](" +
            join(for (kw <- reallyKeywords) yield '"' + kw + '"') +
            ")\n}\n"
    }
}
