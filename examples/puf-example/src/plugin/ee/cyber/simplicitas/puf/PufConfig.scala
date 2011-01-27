package ee.cyber.simplicitas.puf

import ee.cyber.simplicitas.imp._
import ee.cyber.simplicitas.{GenericToken, CommonNode}

import org.eclipse.swt.graphics.Image
import org.eclipse.imp.language.LanguageRegistry


object PufConfig {
    // These four values serve technical purposes, do not touch them.
    val instance = new PufConfig()
    val languageId = "puf_lang"
    val language = LanguageRegistry.findLanguage(languageId)
    val pluginId = "puf_lang"

    /** Insert here default values by color symbols that will be
     * returned by the getTokenColor method. Map key is the color
     * symbol and value is description of the color symbol, consisting of:
     * - human-readable name of the color that is shown in the preferences
     *   dialog.
     * - default value of the color, encoded as "R,G,B"
     *   (e.g., "255, 255, 0" for yellow).
     * - font style using SWT constants (e.g, SWT.BOLD | SWT.ITALIC)
     */
    val colors: Map[Symbol, Tuple3[String, String, Number]] =
        Map.empty

    /** This object contains references to images that are loaded via
     * plugin's ImageRegistry. The image objects are declared in this object
     * and initialized in the <code>initializeImages</code> method. */
    object Images {
        var variable: Image = null
        var function: Image = null
    }

    def initializeImages(addFun: (String, String) => Image) {
        Images.variable = addFun("variable", "icons/variable.gif")
        Images.function = addFun("function", "icons/function.gif")
    }
}

class PufConfig extends APluginConfig {
    import PufConfig.Images

    /** Parses the input using the correct grammar. */
    def parse(ctx: ParseCtx) {
        val grammar = new PufGrammar()

        if (ctx parse grammar) {
            ctx.reportErrors(PufChecker.process(grammar.tree))
        }
    }

    def treeLabel(node: CommonNode) = node match {
        case FunDecl(IdLeft(Id(funName)), FunExpr(_, _)) =>
            funName
        case FunDecl(IdLeft(Id(varName)), _) if isToplevelDef(node) =>
            varName
        case FunExpr(_, _) if !partOfDecl(node) =>
            "(anon)"
        case _ =>
            null
    }

    def isToplevelDef(node: CommonNode) = {
        node.parent.isInstanceOf[Program]
    }

    def partOfDecl(node: CommonNode) =
        node.parent.isInstanceOf[FunDecl]

    def lineCount(node: CommonNode) =
        node.endLine - node.startLine

    override def treeImage(node: CommonNode) = node match {
        case FunDecl(_, FunExpr(_, _)) =>
            Images.function
        case FunDecl(_, _) =>
            Images.variable
        case FunExpr(_, _) =>
            Images.function
        case _ =>
            null
    }

    override def isFoldable(node: CommonNode) = node match {
        case FunDecl(FunLeft(_ :: _ :: _), _)
                | FunExpr(_, _) if lineCount(node) > 3 =>
            true
        case _ =>
            false
    }


    override def referenceTarget(node: CommonNode) = node match {
        case id: Id => id.target
        case _ => null
    }

    override def runGenerator(dir: String, file: String) {
        PufMain.main(Array("--dest", dir, dir + file))
    }

    override def singleLineCommentPrefix = "//"

    override def fences = Array(("(", ")"), ("[", "]"))
}
