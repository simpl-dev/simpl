// Copyright (c) 2010 Cybernetica AS / STACC

package ee.cyber.simplicitas.imp

import ee.cyber.simplicitas.{GenericToken, CommonNode}
import org.eclipse.jface.text.TextAttribute
import org.eclipse.core.resources.IFile
import org.eclipse.swt.graphics.Image
import org.eclipse.swt.SWT

/** This class offers the main entry point for customising the 
  * IDE of the DSL. Override methods in this class to create your
  * own IDE.
  */
abstract class APluginConfig {
    /** Parse the source file. Basically, this method exists to get access
      * to DSL-specific Grammar class. The simplest implementation of this
      * method for language Foo is:<br>
      * <code>ctx.parse(new FooGrammar())</code><br>
      * In addition to parsing, you should perform program validation in
      * this method. For reporting validation errors, use
      * @see ParseCtx#reportErrors method. */
    def parse(ctx: ParseCtx)

    /** Return true, if auto-completion should take the given token into
      * account. In general, this is used to signal that white space,
      * punctuation, operators etc. cannot be completed.
      * <p> 
      * For example, let's assume that the user presses
      * ctrl-space at word "abc|def" ("|" marks cursor position).
      * If this method returns true, Eclipse tries to find completion,
      * starting with prefix "abc". If this method returns false, the
      * completion prefix is "" instead. */
    def canBeCompleted(token: GenericToken) =
        !token.isHidden && !token.isOperator

    /** Find proposals for auto-completion. See the documentation for
      * ProposalCtx for further information. */
    def propose(ctx: ProposalCtx) {
        ctx.addKeywords()
    }

    /** If the node represents a link, then return the object that it points
      * to. This service is used by Eclipse to make some tokens behave as
      * hyperlinks. It is also used by hover helper (for links, the hover text
      * is documentation of the link target). */
    def referenceTarget(node: CommonNode): CommonNode = null

    /** This method is called to determint the text that will be shown
      * in a hover when the node behaves as link.
      * TODO: Currently does noot work as advertised. */
    def getLinkText(node: CommonNode) = String.valueOf(node)

    /** Return text that will be shown as documentation hover for that
      * node. You can use HTML tags in the text. */
    def getDocumentation(node: CommonNode): String = null

    /** Return label that will be shown for that node in the outline
      * view. The default implementation of the <code>addToTree</code>
      * method shows only nodes that have label. */
    def treeLabel(node: CommonNode): String
    /** Return icon that will be used to annotate this node in the
      * outline view. Normally, the images are loaded in 
      * <code>YourLangConfig.loadImages</code> method and stored in the
      * <code>YourLangConfig</code> object. The images must be 16x16 pixels. */
    def treeImage(node: CommonNode): Image = null

    /** Return icon that will be displayed in the editor tab showing
      * this file. Normally, the images are loaded in 
      * <code>YourLangConfig.loadImages</code> method and stored in the
      * <code>YourLangConfig</code> object. The images must be 16x16 pixels. */
    def fileImage(file: IFile): Image = null

    /** Return true, if this node should be shown in the outline view.
      * The default implementation shows nodes for which the
      * <code>treeLabel</code> method returns non-null label. */
    def addToTree(node: CommonNode) = treeLabel(node) ne null

    /** Return true, if this node should be made foldable. The default
      * implementation makes node foldable if it is also shown in the outline
      * view. */
    def isFoldable(node: CommonNode) = addToTree(node)

    /** Returns color symbol for given token (or null, if no coloring).
      * Simpl uses one level of indirection with syntax-coloring service.
      * On the first level, the <code>getTokenColor</code> method returns
      * symbol corresponding to token kind, such as 'comment, 'definition,
      * etc. On the second level, the YourLanguageNameConfig object contains
      * information for each token kind, such as human-readable name,
      * default color and font style.
      * It is important that the two pieces of information are in sync and
      * that the getTokenColor method only return symbols that are described
      * in the table at the YuorLanguageNameConfig object.
      */
    def getTokenColor(token: GenericToken): Symbol = null

    /** Return prefix for single-line comments. This is used to automatically
      * comment-uncomment lines of text. */
    def singleLineCommentPrefix: String = null

    /** Return pair of matching brackets that will be highlighted in the
      * code editor. For example: <code>Array(("(", ")"), ("[", "]"))</code> */
    def fences: Array[Tuple2[String, String]] = Array()
    
    /**
      * Run the code generator for given file. This method is called when
      * the user selects "Generate" from the drop-down menu in Navigator
      * window.
      * @param directory full path to directory where the DSL file is located.
      * @param file file that was selected for generation. File name is
      * relative to the directory parameter.
      */
    def runGenerator(directory: String, file: String) {}
}
