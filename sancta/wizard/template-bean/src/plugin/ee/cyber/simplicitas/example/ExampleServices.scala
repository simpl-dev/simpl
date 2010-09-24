// This file contains generated code. You are not supposed to edit it.
// Instead, use the file #{class}Config.scala to customize your DSL IDE.

package #{package}

import ee.cyber.simplicitas.imp._
import ee.cyber.simplicitas.imp.parser._
import ee.cyber.simplicitas.eclipse.GenerateAction

import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.SWT


object #{class}Plugin {
  var instance: #{class}Plugin = null
  
  def getInstance =
    if (instance eq null)
      new #{class}Plugin()
    else
      instance
  
  val factory = () => getInstance
}

class #{class}Plugin extends SimplicitasPlugin {
  def getID = #{class}Config.pluginId
  def getLanguageID = #{class}Config.languageId

  #{class}Plugin.instance = this
  
  colorCache = collection.mutable.Map()

  colorDefs =
    Map[Symbol, Tuple3[String, String, Number]](
      'keyword -> ("Keywords", "128,0,128", SWT.BOLD),
      'operator -> ("Operators", "0,0,0", SWT.NORMAL),
      'comment -> ("Comments", "128,128,0", SWT.ITALIC)) ++
    #{class}Config.colors

  override def initializeImageRegistry(registry: ImageRegistry) {
    super.initializeImageRegistry(registry)

    val bundle = org.eclipse.core.runtime.Platform.getBundle(
            #{class}Config.pluginId);
    val addFun =
      (key: String, path: String) =>
        addImage(key, path, bundle, registry)
    #{class}Config.initializeImageRegistry(addFun)
  }
}

class #{class}ParseController extends 
  SimplicitasParseController(#{class}Config.language, #{class}Config.instance) {}

class #{class}TokenColorer extends TokenColorerBase(#{class}Plugin.factory,
                                                    #{class}Config.instance) {}

class #{class}TreeModelBuilder
  extends SimplicitasTreeModelBuilder(#{class}Config.instance) {
}

class #{class}LabelProvider
    extends SimplicitasLabelProvider(#{class}Config.instance) {}

class #{class}ReferenceResolver
    extends SimplicitasReferenceResolver(#{class}Config.instance) {}

class #{class}FoldingUpdater
    extends SimplicitasFoldingUpdater(#{class}Config.instance) {}

class #{class}DocumentationProvider
    extends SimplicitasDocumentationProvider(#{class}Config.instance) {}

class #{class}ContentProposer
    extends SimplicitasContentProposer(#{class}Config.instance) {}

class #{class}OccurrenceMarker
    extends SimplicitasOccurrenceMarker(#{class}Config.instance) {}

class #{class}PreferencePage
    extends SimplicitasPreferencePage(#{class}Plugin.factory) {}

class #{class}PreferenceInitializer
    extends SimplicitasPreferenceInitializer(#{class}Plugin.factory) {}

class #{class}GenerateAction extends GenerateAction(#{class}Config.instance) {}