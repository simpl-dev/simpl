// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.ide

import ee.cyber.simplicitas.imp._
import ee.cyber.simplicitas.{GenericToken, CommonToken, CommonNode}

import org.eclipse.imp.language.LanguageRegistry
import org.eclipse.core.resources.IFile
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Image


object SimplConfig {
    val instance = new SimplConfig()
    val languageId = "simpl_ide"
    val language = LanguageRegistry.findLanguage(languageId)
    val pluginId = "simpl_ide"

    /** Here are constants for icons. The se items are initialized in the
      * </code>initializeImageRegistry</code> method. */
    object Images {
        /* Simpl file icon. */
        var simpl: Image = null

        /* Outline view icons. */
        var terminal: Image = null
        var nonTerminal: Image = null
        var fragment: Image = null
        var option: Image = null
    }

    val colors: Map[Symbol, Tuple3[String, String, Number]] =
        Map(
            'strings -> ("Strings", "0, 128, 0", SWT.BOLD | SWT.ITALIC),
            'code -> ("Embedded code", "128, 0, 0", SWT.NORMAL))

    def initializeImages(addFun: (String, String) => Image) {
        import Images._

        simpl = addFun("simpl", "icons/simpl.png")

        terminal = addFun("terminal", "icons/terminal.png")
        nonTerminal = addFun("nonTerminal", "icons/nonterminal.png")
        fragment = addFun("fragment", "icons/fragment.png")
        option = addFun("option", "icons/option.png")
    }
}

class MyGrammar extends SimplGrammar {
    var ctx: SimplCtx = null
}

class SimplConfig extends APluginConfig() {
    import SimplConfig.Images

    // parses using some grammar
    def parse(ctx: ParseCtx) {
        val grammar = new MyGrammar()

        if (ctx parse grammar) {
            grammar.ctx = new SimplCtx(grammar)
            new SimplPostProcess(grammar.ctx).validate()
            ctx.reportErrors(grammar.ctx.errors)
        }
    }

    // labels for nodes that belong to the tree model
    def treeLabel(node: CommonNode) = node match {
        case d: RuleDef => d.name.text
        case _ => null
    }

    override def treeImage(node: CommonNode) = node match {
        case t: TerminalDef => Images.terminal
        case nt: NonTerminalDef => Images.nonTerminal
        case f: FragmentDef => Images.fragment
        case o: OptionDef => Images.option
        case _ => null
    }

    override def fileImage(file: IFile) = Images.simpl

    override def isFoldable(node: CommonNode) = node match {
        // Fold rule definitions if they span 3 or more lines.
        case d: RuleDef if node.endLine - node.startLine > 2 => true
        case _ => false
    }

    override def referenceTarget(node: CommonNode): CommonNode =
        node match {
            case id: Id =>
                id.parent match {
                    case ref: Ref =>
                        if (ref.ref ne null) ref.ref.name else null
                    case _ =>
                        null
                }
            case _ => null
        }

    override def getDocumentation(node: CommonNode): String =
        node match {
            // This case is used when showing documentation of linking nodes.
            case d: RuleDef => d.documentation
            case id: Id =>
                id.parent match {
                    case d: RuleDef if d.name eq id => d.documentation
                    case _ => null
                }
            case _ => null
        }

    override def propose(ctx: ProposalCtx) {
        val simplCtx = ctx.grammar.asInstanceOf[MyGrammar].ctx

        (ctx.node, ctx.parent) match {
            case (Id(foo), RuleRef(_)) =>
                addProposals(ctx, simplCtx.nonTerms)
                addProposals(ctx, simplCtx.options)
                addProposals(ctx, simplCtx.terminals)
            case (Id(foo), FragmentRef(_)) =>
                addProposals(ctx, simplCtx.fragments)
            case _ =>
                ctx.addKeywords()
        }
    }

    // For some reasons, Map is not compatible with immutable.Map.
    def addProposals(ctx: ProposalCtx,
                     symbols: Iterable[(String, RuleDef)]) {
        for ((key, rule) <- symbols) {
            if ((ctx.prefix eq null) ||
                    ctx.prefix == "" ||
                    key.startsWith(ctx.prefix)) {
                ctx.add(rule.name.text, rule.name.text, rule.documentation)
            }
        }
    }

    override def getTokenColor(token: GenericToken): Symbol = {
        val myToken = token.asInstanceOf[CommonToken[SimplKind.Kind]]

        myToken.kind match {
            case SimplKind.Str => 'strings
            case SimplKind.Code => 'code
            case _ => null
        }
    }

    override def singleLineCommentPrefix = "//"

    override def fences =
        Array(("(", ")"),
            ("{", "}"))

    override def runGenerator(dir: String, file: String) {
        SimplMain.main(Array[String]("--dest", dir, dir + file))
    }
}
