package #{package}

import ee.cyber.simplicitas.imp._
import ee.cyber.simplicitas.{GenericToken, CommonNode}

import org.eclipse.imp.language.LanguageRegistry


object #{class}Config {
  // These four values serve technical purposes, do not touch them.
  val instance = new #{class}Config()
  val languageId = "#{lang}"
  val language = LanguageRegistry.findLanguage(languageId)
  val pluginId = "#{id}"

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
}

class #{class}Config extends APluginConfig {
    /** Parses the input using the correct grammar. */
    def parse(ctx: ParseCtx) {
        ctx parse new #{class}Grammar()
    }

    /** The outline will contain nodes corresponding to header,
      * bean and each field. */
    def treeLabel(node: CommonNode) = node match {
        case header: Header => "Header"
        case bean: Bean => bean.id.text
        case field: Field => field.id.text + ": " + field.dotId.text          
        case _ => null
    }

    /** The isFoldable defaults to checking whether tree has a label,
      * but we do not want the fields to be foldable. */
    override def isFoldable(node: CommonNode) = node match {
        case _: Header => true
        case _: Bean => true
        case _ => false
    }

  override def runGenerator(dir: String, file: String) {
    #{class}Main.main(Array("--dest", dir, dir + file))
  }
}
