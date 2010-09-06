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

  /** There is nothing to show in the outline view. */
  def treeLabel(node: CommonNode) = null

  override def runGenerator(dir: String, file: String) {
    #{class}Main.main(Array("--dest", dir, dir + file))
  }
}
